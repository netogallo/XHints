module XMonad.Actions.XHints.Hint where

import XMonad.Actions.XHints.State
import Data.Typeable
import XMonad

runXHint :: Typeable v => (String -> XHint v) -> X ()
runXHint hint = undefined
  
