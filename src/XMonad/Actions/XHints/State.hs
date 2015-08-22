{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TupleSections #-}
module XMonad.Actions.XHints.State where

import XMonad hiding (liftX)
import qualified Data.Map as M
import Data.Typeable
import GHC.Fingerprint.Type
import Control.Applicative (Applicative(..),(<$>))
import Control.Monad       (liftM, ap)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.ByteString (ByteString)

data ActionState = ActionState {
  window :: Maybe (Window,GC)
  } deriving Typeable

data XHintsState = XHintsState{
  actions :: M.Map TypeRep ActionState
  } deriving Typeable

data XHintConf = XHintConf {}

newtype XHints v a = XHints{runXHints :: XHintsState -> X (a,ActionState)}

type XHint v = XHints v (Either String String)

emptyState = ActionState{window = Nothing}

instance ExtensionClass XHintsState where
  initialValue = XHintsState {actions = M.empty}

instance Typeable v => Functor (XHints v) where
  fmap = liftM
 
instance Typeable v => Applicative (XHints v) where
    pure  = return
    (<*>) = ap

instance Typeable v => Monad (XHints v) where
  m >>= f = XHints $ \state -> do
    (res,as) <- runXHints m state
    let state' = state{actions = M.insert rep as (actions state)}
        rep = typeOf (undefined :: v)
    runXHints (f res) state'
  return a = XHints $ \s -> return (a,actions s M.! (typeOf (undefined :: v)))

instance Typeable v => MonadIO (XHints v) where
  liftIO = liftX . liftIO

hintState :: forall v . Typeable v => XHints v ActionState
hintState = XHints $ \s -> do
  let s' = case M.lookup (typeOf (undefined :: v)) (actions s) of
        Nothing -> emptyState
        Just s -> s
  return (s',s')
  
liftX :: Typeable v => X a -> XHints v a
liftX x = do
  s <- hintState
  XHints $ \_ -> (,s) <$> x
