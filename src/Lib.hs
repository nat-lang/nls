module Lib where

import Data.Maybe (fromJust)
import qualified Language.LSP.Types as J

normalizedUriToFilePath = fromJust . J.uriToFilePath . J.fromNormalizedUri