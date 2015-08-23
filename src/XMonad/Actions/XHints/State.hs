{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TupleSections, ExistentialQuantification #-}
module XMonad.Actions.XHints.State where

import XMonad hiding (liftX)
import qualified Data.Map as M
import Data.Typeable
import GHC.Fingerprint.Type
import Control.Applicative (Applicative(..),(<$>))
import Control.Monad       (liftM, ap)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Control.Monad.State.Class
import Control.Monad.State.Strict

data Store = forall a . Typeable a => Store (Maybe a)

data ActionState = ActionState {
  window :: Maybe (Window,GC)
  } deriving Typeable

data HintState = forall s . HintState s

data XHintsState = XHintsState{
  actions :: M.Map TypeRep (ActionState, Store)
  } deriving Typeable

data XHintConf = XHintConf {}

newtype XHints v a = XHints{runXHints :: ActionState -> X (a,ActionState)}

type XHint s v = StateT (Maybe s) (XHints v) (Either Text Text)

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
    let state' = as -- state{actions = M.insert rep as (actions state)}
        rep = typeOf (undefined :: v)
    runXHints (f res) state'
  return a = XHints $ \s -> return (a,s) -- actions s M.! (typeOf (undefined :: v)))

instance Typeable v => MonadIO (XHints v) where
  liftIO = liftX . liftIO

hintState :: forall v . Typeable v => XHints v ActionState
hintState = XHints $ \s -> return (s,s)
  
liftX :: Typeable v => X a -> XHints v a
liftX x = do
  s <- hintState
  XHints $ \_ -> (,s) <$> x
