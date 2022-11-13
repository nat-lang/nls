{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
-- So we can keep using the old prettyprinter modules (which have a better
-- compatibility range) for now.
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- This is a language server built with haskell-lsp using a 'Reactor'
-- design. With a 'Reactor' all requests are handled on a /single thread/.
-- A thread is spun up for it, which repeatedly reads from a 'TChan' of
-- 'ReactorInput's.
-- The `lsp` handlers then simply pass on all the requests and
-- notifications onto the channel via 'ReactorInput's.
-- This way there is the option of executing requests on multiple threads, without
-- blocking server communication.
module Reactor (runReactor) where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import qualified Colog.Core as L
import Control.Concurrent
import Control.Concurrent.STM.TChan
import qualified Control.Exception as E
import Control.Lens hiding (Iso)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM
import qualified Data.Aeson as J
import Data.Int (Int32)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TiO
import Data.Text.Prettyprint.Doc
import Data.Word
import Diagnostics.Document
import GHC.Generics (Generic)
import Language.LSP.Diagnostics
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Server
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import Language.LSP.VFS
import System.Exit
import System.IO

runReactor :: IO ()
runReactor = do
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- ---------------------------------------------------------------------

data Config = Config {enabled :: Bool, options :: Map.Map String String}
  deriving (Generic, J.ToJSON, J.FromJSON, Show)

run :: IO Int
run = flip E.catches handlers $ do
  rin <- atomically newTChan :: IO (TChan ReactorInput)

  let -- Three loggers:
      -- 1. To stderr
      -- 2. To the client (filtered by severity)
      -- 3. To both
      stderrLogger :: LogAction IO (WithSeverity T.Text)
      stderrLogger = L.cmap show L.logStringStderr
      clientLogger :: LogAction (LspM ()) (WithSeverity T.Text)
      clientLogger = defaultClientLogger
      dualLogger :: LogAction (LspM ()) (WithSeverity T.Text)
      dualLogger = clientLogger <> L.hoistLogAction liftIO stderrLogger

      serverDefinition =
        ServerDefinition
          { defaultConfig = (),
            onConfigurationChange = \_old v -> do
              case J.fromJSON v of
                J.Error e -> Left (T.pack e)
                J.Success cfg -> Right cfg,
            doInitialize = \env _ -> forkIO (reactor stderrLogger rin) >> pure (Right env),
            -- Handlers log to both the client and stderr
            staticHandlers = lspHandlers dualLogger rin,
            interpretHandler = \env -> Iso (runLspT env) liftIO,
            options = lspOptions
          }

  let logToText = T.pack . show . pretty
  runServerWithHandles
    -- Log to both the client and stderr when we can, stderr beforehand
    (L.cmap (fmap logToText) stderrLogger)
    (L.cmap (fmap logToText) dualLogger)
    stdin
    stdout
    serverDefinition
  where
    handlers =
      [ E.Handler ioExcept,
        E.Handler someExcept
      ]
    ioExcept (e :: E.IOException) = print e >> return 1
    someExcept (e :: E.SomeException) = print e >> return 1

-- ---------------------------------------------------------------------

syncOptions :: J.TextDocumentSyncOptions
syncOptions =
  J.TextDocumentSyncOptions
    { J._openClose = Just True,
      J._change = Just J.TdSyncIncremental,
      J._willSave = Just False,
      J._willSaveWaitUntil = Just False,
      J._save = Just $ J.InR $ J.SaveOptions $ Just False
    }

lspOptions :: Options
lspOptions =
  defaultOptions
    { textDocumentSync = Just syncOptions,
      executeCommandCommands = Just []
    }

-- ---------------------------------------------------------------------

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.

newtype ReactorInput = ReactorAction (IO ())

normalizedUriToFilePath = fromJust . J.uriToFilePath . J.fromNormalizedUri

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: J.NormalizedUri -> T.Text -> Maybe Int32 -> LspM () ()
sendDiagnostics uri doc version = do
  diags <- diagnoseDoc doc
  sendNotification J.SWindowShowMessage $
    J.ShowMessageParams J.MtInfo (T.pack $ show diags)
  publishDiagnostics 100 uri version (partitionBySource diags)

sendFileDiagnostics :: J.NormalizedUri -> Maybe Int32 -> LspM () ()
sendFileDiagnostics uri version = do
  doc <- liftIO $ TiO.readFile (normalizedUriToFilePath uri)
  sendDiagnostics uri doc version

sendVirtualFileDiagnostics :: J.NormalizedUri -> VirtualFile -> LspM () ()
sendVirtualFileDiagnostics uri vf = do
  let doc = virtualFileText vf
      version = virtualFileVersion vf
  sendDiagnostics uri doc (Just version)

-- ---------------------------------------------------------------------

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: L.LogAction IO (WithSeverity T.Text) -> TChan ReactorInput -> IO ()
reactor logger inp = do
  logger <& "Started the reactor" `WithSeverity` Info
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: (m ~ LspM ()) => L.LogAction m (WithSeverity T.Text) -> TChan ReactorInput -> Handlers m
lspHandlers logger rin = mapHandlers goReq goNot (handle logger)
  where
    goReq :: forall (a :: J.Method J.FromClient J.Request). Handler (LspM ()) a -> Handler (LspM ()) a
    goReq f = \msg k -> do
      env <- getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg k)

    goNot :: forall (a :: J.Method J.FromClient J.Notification). Handler (LspM ()) a -> Handler (LspM ()) a
    goNot f = \msg -> do
      env <- getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg)

-- | Where the actual logic resides for handling requests and notifications.
handle :: (m ~ LspM ()) => L.LogAction m (WithSeverity T.Text) -> Handlers m
handle logger =
  mconcat
    [ notificationHandler J.SInitialized $ \_msg ->
        logger <& "Processing the Initialized notification" `WithSeverity` Info,
      notificationHandler J.STextDocumentDidOpen $ \msg -> do
        let doc = msg ^. J.params . J.textDocument . J.uri
            fileName = J.uriToFilePath doc
        logger <& ("Processing DidOpenTextDocument for: " <> T.pack (show fileName)) `WithSeverity` Info
        sendFileDiagnostics (J.toNormalizedUri doc) (Just 0),
      notificationHandler J.SWorkspaceDidChangeConfiguration $ \msg -> do
        logger L.<& ("Configuration changed: " <> T.pack (show msg)) `WithSeverity` Info,
      notificationHandler J.STextDocumentDidChange $ \msg -> do
        let uri = msg ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
        logger <& ("Processing DidChangeTextDocument for: " <> T.pack (show uri)) `WithSeverity` Info
        mdoc <- getVirtualFile uri
        case mdoc of
          Just vf@(VirtualFile _version str _) -> do
            logger <& ("Found the virtual file: " <> T.pack (show str)) `WithSeverity` Info
            sendVirtualFileDiagnostics uri vf
          Nothing -> do
            logger <& ("Didn't find anything in the VFS for: " <> T.pack (show uri)) `WithSeverity` Info,
      notificationHandler J.STextDocumentDidSave $ \msg -> do
        let doc = msg ^. J.params . J.textDocument . J.uri
            fileName = J.uriToFilePath doc
        logger <& ("Processing DidSaveTextDocument  for: " <> T.pack (show fileName)) `WithSeverity` Info
        sendFileDiagnostics (J.toNormalizedUri doc) Nothing,
      requestHandler J.STextDocumentRename $ \req responder -> do
        logger <& "Processing a textDocument/rename request" `WithSeverity` Info
        let params = req ^. J.params
            J.Position l c = params ^. J.position
            newName = params ^. J.newName
        vdoc <- getVersionedTextDoc (params ^. J.textDocument)
        -- Replace some text at the position with what the user entered
        let edit = J.InL $ J.TextEdit (J.mkRange l c l (c + fromIntegral (T.length newName))) newName
            tde = J.TextDocumentEdit vdoc (J.List [edit])
            -- "documentChanges" field is preferred over "changes"
            rsp = J.WorkspaceEdit Nothing (Just (J.List [J.InL tde])) Nothing
        responder (Right rsp),
      requestHandler J.STextDocumentHover $ \req responder -> do
        logger <& "Processing a textDocument/hover request" `WithSeverity` Info
        let J.HoverParams _doc pos _workDone = req ^. J.params
            J.Position _l _c' = pos
            rsp = J.Hover ms (Just range)
            ms = J.HoverContents $ J.markedUpContent "lsp-hello" "Your type info here!"
            range = J.Range pos pos
        responder (Right $ Just rsp),
      requestHandler J.STextDocumentDocumentSymbol $ \req responder -> do
        logger <& "Processing a textDocument/documentSymbol request" `WithSeverity` Info
        let J.DocumentSymbolParams _ _ doc = req ^. J.params
            loc = J.Location (doc ^. J.uri) (J.Range (J.Position 0 0) (J.Position 0 0))
            sym = J.SymbolInformation "lsp-hello" J.SkFunction Nothing Nothing loc Nothing
            rsp = J.InR (J.List [sym])
        responder (Right rsp),
      requestHandler J.STextDocumentCodeAction $ \req responder ->
        logger <& "Processing a textDocument/codeAction request" `WithSeverity` Info,
      requestHandler J.SWorkspaceExecuteCommand $ \req responder -> do
        logger <& "Processing a workspace/executeCommand request" `WithSeverity` Info
        let params = req ^. J.params
            margs = params ^. J.arguments

        logger <& ("The arguments are: " <> T.pack (show margs)) `WithSeverity` Debug
        responder (Right (J.Object mempty)) -- respond to the request
        void $
          withProgress "Executing some long running command" Cancellable $ \update ->
            forM [(0 :: J.UInt) .. 10] $ \i -> do
              update (ProgressAmount (Just (i * 10)) (Just "Doing stuff"))
              liftIO $ threadDelay (1 * 1000000),
      notificationHandler J.SCancelRequest $ \_msg -> do
        logger <& ("Received a request to cancel: " <> T.pack (show _msg)) `WithSeverity` Debug
    ]

-- ---------------------------------------------------------------------