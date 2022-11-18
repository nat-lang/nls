{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Render where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import qualified Colog.Core as L
import Control.Concurrent (ThreadId, forkFinally, forkIO)
import Control.Exception (SomeException (SomeException))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM
import qualified Data.Text as T
import qualified Data.Text.IO as TiO
import Diagnostics.Parsing
import Diagnostics.Typing
import Language.LSP.Server
import qualified Language.LSP.Types as J
import Lib (normalizedUriToFilePath)
import qualified Nat.Evaluation.Module as E
import Nat.Parser
import qualified Nat.Syntax.Module as S
import Nat.TeX
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (-<.>))
import System.Process (proc, readCreateProcessWithExitCode, readProcess, readProcessWithExitCode, system)

renderDocument ::
  (m ~ LspM ()) =>
  L.LogAction m (WithSeverity T.Text) ->
  J.NormalizedUri ->
  T.Text ->
  LspM () ()
renderDocument logger uri doc = do
  exitCode <- liftIO render
  handle exitCode
  where
    file = normalizedUriToFilePath uri
    tFile = T.pack $ show file
    pdfUri = J.filePathToUri (file -<.> "pdf")

    handle :: ExitCode -> LspM () ()
    handle exitCode = case exitCode of
      ExitFailure code -> logger <& ("Unexpected error while rendering " <> tFile <> ": errno " <> T.pack (show code)) `WithSeverity` Error
      ExitSuccess -> do
        logger <& ("Completed rendering: " <> tFile) `WithSeverity` Info
        let params = J.ShowDocumentParams pdfUri Nothing Nothing Nothing
        void $
          sendRequest J.SWindowShowDocument params $ \case
            Left e -> logger <& ("Got an error: " <> T.pack (show e)) `WithSeverity` Error
            Right _ -> logger <& "Ok" `WithSeverity` Info

    render :: IO ExitCode
    render = case S.runPModule doc of
      Left {} -> return $ ExitFailure 1
      Right mod -> do
        let texDir = takeDirectory file
        let texFile = file -<.> "tex"
        typesetFile texFile mod
        (exitCode, _, _) <- readProcessWithExitCode "pdflatex" ["-output-directory=" <> texDir] texFile
        return exitCode
