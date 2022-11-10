{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}

module Diagnostics.Parsing where

import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Void
import Data.Word
import Diagnostics.Diagnose
import Language.LSP.Server
import qualified Language.LSP.Types as J
import Language.LSP.VFS
import Nat.Evaluation.Module
import Nat.Parser
import Nat.Syntax.Module
import qualified Text.Megaparsec as P

type Position = (J.UInt, J.UInt)

errPosition :: P.PosState s -> Position
errPosition (P.PosState _ _ (P.SourcePos _ line col) _ _) = (pos line, pos col)
  where
    pos :: P.Pos -> J.UInt
    pos = fromIntegral . P.unPos

join sep = foldl1 (\x y -> x ++ sep ++ y)

pJoin sep = T.pack . join sep

packErr :: P.ParseError T.Text Void -> T.Text
packErr (P.TrivialError _ mUnexpected expected) = T.pack $ case mUnexpected of
  Nothing -> ""
  Just unexpected -> concat ["Expecting one of: ", expected', ". Got: ", showErrItem unexpected]
  where
    expected' = intercalate " | " (fmap showErrItem (Set.toList expected))
    showErrItem item = case item of
      P.Tokens (x :| xs) -> x : xs
      P.Label (c :| cs) -> c : cs
      P.EndOfInput -> "EOF"
packErr (P.FancyError _ errors) = pJoin " " (showErr <$> Set.toList errors)
  where
    showErr e = case e of
      P.ErrorFail msg -> msg
      P.ErrorIndentation ord p0 p1 -> join " " [show p0, show ord, show p1]

errOffset = \case
  P.TrivialError o _ _ -> o
  P.FancyError o _ -> o

instance Diagnosable (P.ParseError T.Text Void) where
  diagnose range e =
    return $
      J.Diagnostic
        range
        (Just J.DsError)
        (Just (J.InL 42))
        (Just serverName)
        (packErr e)
        Nothing
        Nothing
    where
      offset = fromIntegral $ case e of
        P.TrivialError _ (Just unexpected) _ -> length (show unexpected)
        _ -> 0

diagnoseErrBundle :: P.ParseErrorBundle T.Text Void -> LspM () [J.Diagnostic]
diagnoseErrBundle (P.ParseErrorBundle (e :| es) state) = mapM diagnose' (e : es)
  where
    (_, state') = P.reachOffset (errOffset e) state
    (linePos, colPos) = errPosition state'
    linePos' = linePos - 1 -- megaparsec is 0-based, lsp 1-based
    range = J.mkRange linePos' 0 linePos' colPos
    diagnose' = diagnose range
