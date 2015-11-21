{-# Language RecordWildCards, ScopedTypeVariables, DeriveDataTypeable, FlexibleInstances, ConstraintKinds #-}
module XMonad.Actions.XHints.Hint where

import XMonad.Actions.XHints.State
import Data.Typeable
import XMonad
import qualified Data.Map as M
import Data.Typeable
import qualified Data.Text as T
import Codec.Binary.UTF8.String (decode)
import Control.Exception.Extensible as E (catch,SomeException(..))
import Data.Maybe (fromMaybe)
import Control.Monad.State.Strict
import Control.Monad.Except (MonadError(..), throwError, ExceptT, runExceptT)
import Control.Concurrent.MVar  
import XMonad.Actions.XHints.Render (newHintWindow)

type XHintRunner a = ExceptT String X a

-- | The type used to index the XHints XState entry
xHintsTy = typeOf (undefined :: XHintsState)

kXHintsTy = show xHintsTy

-- | Returns a String corresponding to the current mouse selection in X;
--   if there is none, an empty string is returned.
getSelection :: MonadIO m => m String
getSelection = io $ do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
  rootw  <- rootWindow dpy dflt
  win <- createSimpleWindow dpy rootw 0 0 1 1 0 0 0
  p <- internAtom dpy "PRIMARY" True
  ty <- E.catch
               (E.catch
                     (internAtom dpy "UTF8_STRING" False)
                     (\(E.SomeException _) -> internAtom dpy "COMPOUND_TEXT" False))
             (\(E.SomeException _) -> internAtom dpy "sTring" False)
  clp <- internAtom dpy "BLITZ_SEL_STRING" False
  xConvertSelection dpy p ty clp win currentTime
  allocaXEvent $ \e -> do
    nextEvent dpy e
    ev <- getEvent e
    if ev_event_type ev == selectionNotify
       then do res <- getWindowProperty8 dpy clp win
               return $ decode . map fromIntegral . fromMaybe [] $ res
       else destroyWindow dpy win >> return ""

-- | Read from the XMonad context the map that contains teh state of
-- all XHints extensions
getState :: forall status store . (Typeable status, Typeable store) => Ty status -> Maybe store -> X XHintsState
getState _  def = do
  xstate@XState {extensibleState = ex} <- get
  let xHintsState = case M.lookup kXHintsTy ex of
        Nothing -> (initialValue :: XHintsState)
        Just (Right (StateExtension s)) -> (\(Just x) -> x) $ cast s
        x -> undefined
  case M.lookup ident (actions xHintsState) of
    -- If no state for the current action extists, create it and
    -- update the XState
    Nothing -> do
      actionStateVar <- liftIO $ newMVar (initialValue, Store def)
      let updatedMappings = M.insert ident actionStateVar (actions xHintsState)
      let extension = Right (StateExtension (xHintsState{actions=updatedMappings}))
      put xstate{extensibleState = M.insert kXHintsTy  extension ex}
      return xHintsState{actions = updatedMappings}
    Just xHintState ->  return xHintsState

      -- Otherwise just return the existing state and mappings
      -- Just s -> return (actions xHintsState)
  where
    ident = typeOf (undefined :: status)

-- | Get the current status of a particular XHint plugin. Fail if the plugin is not found
-- in the list of status. If this fails, it indcates a bug in the program.
getHintStatusVar :: forall storeTy statusTy m  . (Typeable statusTy, Typeable storeTy) => Ty statusTy -> Maybe storeTy -> XHintRunner (MVar (XHintsStatus, Store))
getHintStatusVar ty def = do
  xHintState <- lift $ getState ty def
  case M.lookup statusIdent (actions xHintState) of
    Nothing -> throwError $ "The type " ++ show statusIdent ++ " is not in the mappings."
    Just statusVar -> return statusVar
  where
      statusIdent = typeOf (undefined :: statusTy)

getHintStatus :: forall storeTy  . (Typeable storeTy) => (MVar (XHintsStatus, Store)) -> XHintRunner (XHintsStatus, Maybe storeTy)
getHintStatus statusVar = do
  (status, Store store) <- liftIO $ readMVar statusVar
  case (cast store :: Maybe (Maybe storeTy)) of
    Just store' -> return (status, store')
    Nothing -> throwError $ "The store type " ++ show (typeOf (undefined :: storeTy)) ++ "did not match the parameter."
                    
-- | Action that creates a context to run an XHints Action
createXHintDefaultContext :: X (XHintsContext)
createXHintDefaultContext = do
  dpy <- asks display
  (win,gc) <- liftIO $ newHintWindow dpy
  
  return $ XHintsContext{
    hintWindow = win,
    hintDisplay = dpy,
    graphicsContext = gc}
  
runXHintError :: forall statusTy storeTy error . (Typeable statusTy, Typeable storeTy) => (T.Text -> XHint statusTy storeTy ()) -> XHintRunner ()
runXHintError hint = do
  statusVar <- getHintStatusVar (Ty :: Ty statusTy) (Nothing :: Maybe storeTy)
  (actionState, store) <- getHintStatus statusVar
  (newActionState, updatedStore) <- case context actionState of
    Just XHintsContext{hintWindow = w, hintDisplay = dpy} -> do
      trace "destroying" 
      liftIO $ destroyWindow dpy w
      return $ (actionState{context=Nothing}, store)
    Nothing -> do
      sel <- liftM T.pack getSelection
      ctx <- lift createXHintDefaultContext
      ( (_, context), store') <- liftIO $ runStateT (runXHints (hint sel) ctx) store
      return $ (actionState{context=Just context}, store')
  liftIO $ modifyMVar_ statusVar $ \_ -> return (newActionState, Store updatedStore)
  where
    ident = typeOf (undefined :: statusTy)

runXHint hint = do
  result <- runExceptT $ runXHintError hint
  case result of
    Left error -> trace error
    _ -> return ()
