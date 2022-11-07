{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.IO.Class
import Data.List (isSuffixOf)
import qualified Data.Text as T
import Language.LSP.Server
import Language.LSP.Types
import Nat.Evaluation.Module
import UnliftIO
import UnliftIO.Concurrent

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
            TextDocumentItem uri _ _ _ = doc
            Just fp = uriToFilePath uri
            diag =
              Diagnostic
                (mkRange 0 0 0 1)
                (Just DsWarning)
                (Just (InL 42))
                (Just "dummy-server")
                "Here's a warning"
                Nothing
                Nothing
        withRunInIO $
          \runInIO -> do
            when (".hs" `isSuffixOf` fp) $
              void $
                forkIO $
                  do
                    threadDelay (2 * 10 ^ 6)
                    runInIO $
                      sendNotification STextDocumentPublishDiagnostics $
                        PublishDiagnosticsParams uri Nothing (List [diag]),
      -- notificationHandler STextDocumentDidChange $
      requestHandler STextDocumentHover $ \req responder -> do
        let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
            Position _l _c' = pos
            rsp = Hover ms (Just range)
            ms = HoverContents $ markedUpContent "lsp-demo-simple-server" "Hello world"
            range = Range pos pos
        responder (Right $ Just rsp)
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