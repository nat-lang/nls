{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.List (isSuffixOf)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TiO
import Data.Word
import Language.LSP.Server
import Language.LSP.Types
import Nat.Evaluation.Module
import Nat.Parser
import Nat.Syntax.Module
import Text.Megaparsec (ErrorItem (..), ParseError (TrivialError), ParseErrorBundle (..), Pos, PosState (..), SourcePos (..), Token, unPos)
import UnliftIO
import UnliftIO.Concurrent

suff = ".nl"

class Diagnosable a where
  diagnose :: a -> Diagnostic

errPosition :: PosState s -> (UInt, UInt)
errPosition (PosState _ _ (SourcePos _ line col) _ _) = (pos line, pos col)
  where
    pos :: Pos -> UInt
    pos = fromIntegral . unPos

errTokens :: ParseError T.Text e -> (T.Text, [T.Text])
errTokens (TrivialError _ mUnexpected expected) = (maybe (T.pack "") toStr mUnexpected, expected')
  where
    toStr (Tokens ts) = T.pack (toList ts)
    expected' = fmap toStr (Set.toList expected)

instance Diagnosable (ParseErrorBundle T.Text e) where
  diagnose (ParseErrorBundle (err :| _) state) =
    Diagnostic
      range
      (Just DsInfo)
      (Just (InL 42))
      (Just "nls")
      (T.pack $ "Unexpected: " ++ show unexpected ++ ". Expecting one of: " ++ show expected)
      Nothing
      Nothing
    where
      (line, col) = errPosition state
      range = mkRange line col line (col + unexpectedLen)
      unexpectedLen = fromIntegral $ T.length unexpected
      (unexpected, expected) = errTokens err

sendDiagnostic :: FilePath -> Uri -> Diagnostic -> LspT () IO ()
sendDiagnostic fp uri diag = withRunInIO $
  \runInIO -> do
    when (suff `isSuffixOf` fp) $
      void $
        forkIO $
          do
            threadDelay (2 * 10 ^ 6)
            runInIO $
              sendNotification STextDocumentPublishDiagnostics $
                PublishDiagnosticsParams uri Nothing (List [diag])

documentParseDiagnostic :: FilePath -> LspT () IO Diagnostic
documentParseDiagnostic fp = withRunInIO $
  \runInIO -> do
    doc <- TiO.readFile fp
    return $ case runPModule doc of
      Left err -> diagnose err
      Right {} ->
        Diagnostic
          (mkRange 0 0 0 1)
          (Just DsInfo)
          (Just (InL 42))
          (Just "nls")
          "Looking good!"
          Nothing
          Nothing

-- typeCheckDocument

docPath doc =
  let TextDocumentItem uri _ _ _ = doc
      (Just fp) = uriToFilePath uri
   in (uri, fp)

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SInitialized $ \_not -> do
        let params =
              ShowMessageRequestParams
                MtInfo
                "Turn on code lenses?"
                (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
        _ <- sendRequest SWindowShowMessageRequest params $ \case
          Right (Just (MessageActionItem "Turn on")) -> do
            let regOpts = CodeLensRegistrationOptions Nothing Nothing (Just False)

            _ <- registerCapability STextDocumentCodeLens regOpts $ \_req responder -> do
              let cmd = Command "Say hello" "lsp-hello-command" Nothing
                  rsp = List [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
              responder (Right rsp)
            pure ()
          Right _ ->
            sendNotification SWindowShowMessage (ShowMessageParams MtInfo "Not turning on code lenses")
          Left err ->
            sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Something went wrong!\n" <> T.pack (show err))
        pure (),
      notificationHandler STextDocumentDidOpen $ \noti -> do
        let NotificationMessage _ _ (DidOpenTextDocumentParams doc) = noti
            (uri, fp) = docPath doc
        diag <- documentParseDiagnostic fp
        sendDiagnostic fp uri diag,
      notificationHandler STextDocumentDidChange $ \noti -> do
        let NotificationMessage _ _ (DidChangeTextDocumentParams vDoc _) = noti
            (VersionedTextDocumentIdentifier uri _) = vDoc
            (Just fp) = uriToFilePath uri
        diag <- documentParseDiagnostic fp
        sendDiagnostic fp uri diag,
      requestHandler STextDocumentHover $ \req resp -> do
        let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
            Position _l _c' = pos
            rsp = Hover ms (Just range)
            ms = HoverContents $ markedUpContent "lsp-demo-simple-server" "Hello world"
            range = Range pos pos
        resp (Right $ Just rsp)
    ]

main :: IO Int
main =
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ const $ Right (),
        defaultConfig = (),
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = handlers,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions
      }