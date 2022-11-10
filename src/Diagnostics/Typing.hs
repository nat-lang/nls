{-# LANGUAGE OverloadedStrings #-}

module Diagnostics.Typing where

import qualified Data.Text as T
import Diagnostics.Diagnose
import Language.LSP.Server
import qualified Language.LSP.Types as J
import Nat.Inference
import Nat.Syntax.Surface
import Nat.Syntax.Type

diagnoseTyErr :: InferenceError Type Expr -> LspM () [J.Diagnostic]
diagnoseTyErr err = return [diag]
  where
    diag =
      J.Diagnostic
        (J.mkRange 0 0 0 1)
        (Just J.DsError)
        (Just (J.InL 42))
        (Just serverName)
        (T.pack $ show err)
        Nothing
        Nothing
