{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}

module Diagnostics where

import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word
import qualified Language.LSP.Types as J
import Nat.Evaluation.Module
import Nat.Parser
import Nat.Syntax.Module
import qualified Text.Megaparsec as P

serverName = "nls"

class Diagnosable a where
  diagnose :: a -> J.Diagnostic

errPosition :: P.PosState s -> (J.UInt, J.UInt)
errPosition (P.PosState _ _ (P.SourcePos _ line col) _ _) = (pos line, pos col)
  where
    pos :: P.Pos -> J.UInt
    pos = fromIntegral . P.unPos

errTokens :: P.ParseError T.Text e -> (T.Text, [T.Text])
errTokens (P.TrivialError _ mUnexpected expected) = (maybe (T.pack "") toStr mUnexpected, expected')
  where
    toStr (P.Tokens ts) = T.pack (toList ts)
    expected' = fmap toStr (Set.toList expected)

instance Diagnosable (P.ParseErrorBundle T.Text e) where
  diagnose (P.ParseErrorBundle (err :| _) state) =
    J.Diagnostic
      range
      (Just J.DsError)
      (Just (J.InL 42))
      (Just serverName)
      (T.pack $ "Unexpected: " ++ show unexpected ++ ". Expecting one of: " ++ show expected)
      Nothing
      Nothing
    where
      (line, col) = errPosition state
      range = J.mkRange line col line (col + unexpectedLen)
      unexpectedLen = fromIntegral $ T.length unexpected
      (unexpected, expected) = errTokens err

instance Diagnosable (Either (P.ParseErrorBundle T.Text e) a) where
  diagnose = \case
    Left err -> diagnose err
    Right {} ->
      J.Diagnostic
        (J.mkRange 0 0 0 1)
        (Just J.DsInfo)
        Nothing
        (Just serverName)
        "Ok."
        Nothing
        (Just (J.List []))

instance Diagnosable T.Text where
  diagnose = diagnose . runPModule
