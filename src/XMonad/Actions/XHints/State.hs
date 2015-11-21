{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TupleSections, ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses #-}
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
import Control.Concurrent.MVar (MVar)

-- | Helper type used to pass arguments when only the type
-- of the argument is needed but not the value.
data Ty a = Ty

data Store = forall a . Typeable a => Store (Maybe a)

-- | Contains the values that are given as arguments to the
-- action that produces the hint text
data XHintsContext = XHintsContext {
  hintWindow :: Window,
  graphicsContext :: GC,
  hintDisplay :: Display
  } deriving Typeable

-- | Used by XHints to keep track of the status of each particular
-- XHints plugin
data XHintsStatus = XHintsStatus{
  -- | The context of the hint window or Nothing if the hinting
  -- window is not displayed
  context :: Maybe XHintsContext
  } deriving Typeable

instance ExtensionClass XHintsStatus where
  initialValue = XHintsStatus{context = Nothing}

data HintState = forall s . HintState s

-- | Keeps track of the state of all Hint actions
data XHintsState = XHintsState{
  actions :: M.Map TypeRep (MVar (XHintsStatus, Store))
  } deriving Typeable

instance ExtensionClass XHintsState where
  initialValue = XHintsState{actions = M.empty}
             
data XHintConf = XHintConf {}

newtype XHint v state store = XHint{runXHints :: XHintsContext -> StateT (Maybe state) IO (store, XHintsContext)}

-- type XHint v s a = StateT (Maybe s) (XHints v) a

instance Typeable v => Functor (XHint v s) where
  fmap = liftM
 
instance Typeable v => Applicative (XHint v s) where
    pure  = return
    (<*>) = ap

instance Typeable v => Monad (XHint v s) where
  m >>= f = XHint $ \state -> do
    (res,as) <- runXHints m state
    let state' = as -- state{actions = M.insert rep as (actions state)}
        rep = typeOf (undefined :: v)
    runXHints (f res) state'
  return a = XHint $ \s -> return (a,s) -- actions s M.! (typeOf (undefined :: v)))

instance Typeable v => MonadIO (XHint v s) where
  liftIO io =  XHint $ \a -> liftIO io >>= \v ->  return (v,a)

hintContext :: forall v s . Typeable v => XHint v s XHintsContext
hintContext = XHint $ \ctx -> return (ctx,ctx)

instance Typeable v =>  MonadState (Maybe state) (XHint v state) where
  get = XHint $ \ctx -> do
    v <- get
    return (v, ctx)
  put v = XHint $ \ctx -> put v >> return ((),ctx)
  
