module Diagnostics.Diagnose where

import qualified Data.Text as T
import Language.LSP.Server
import qualified Language.LSP.Types as J

serverName = T.pack "nls"

class Diagnosable a where
  diagnose :: J.Range -> a -> LspM () J.Diagnostic
