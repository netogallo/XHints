{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module XMonad.Actions.XHints.State where

import XMonad
import qualified Data.Map as M
import Data.Typeable
import GHC.Fingerprint.Type

data ActionState = ActionState {
  window :: Maybe Window
  } deriving Typeable

data XHintsState = XHintsState{
  actions :: M.Map TypeRep ActionState
  } deriving Typeable

newtype XHints v a = XHints{runXHints :: XHintsState -> X (a,ActionState)}

instance Typeable v => Monad (XHints v) where
  m >>= f = XHints $ \state -> do
    (res,as) <- runXHints m state
    let state' = state{actions = M.insert rep as (actions state)}
        rep = typeOf (undefined :: v)
    runXHints (f res) state'
  return a = XHints $ \s -> return (a,actions s M.! (typeOf (undefined :: v)))

data XHintConf = XHintConf {}
