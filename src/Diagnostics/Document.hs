{-# LANGUAGE LambdaCase #-}

module Diagnostics.Document where

import qualified Data.Text as T
import Diagnostics.Parsing
import Diagnostics.Typing
import Language.LSP.Server
import qualified Language.LSP.Types as J
import qualified Nat.Evaluation.Module as E
import qualified Nat.Syntax.Module as S

diagnoseDoc :: T.Text -> LspM () [J.Diagnostic]
diagnoseDoc doc = case S.runPModule doc of
  Left errBundle -> diagnoseErrBundle errBundle
  Right mod -> do
    let eTypeEnv = E.typeMod' mod 0
    case eTypeEnv of
      Left err -> diagnoseTyErr err
      Right {} -> return []
