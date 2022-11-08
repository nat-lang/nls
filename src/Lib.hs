{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Control.Monad
import Control.Monad.IO.Class
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TiO
import Language.LSP.Server
import Language.LSP.Types
import Nat.Evaluation.Module
import Nat.Parser
import Nat.Syntax.Module
import Text.Megaparsec
import UnliftIO
import UnliftIO.Concurrent
