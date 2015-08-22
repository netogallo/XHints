{-# Language RecordWildCards, ScopedTypeVariables, DeriveDataTypeable, FlexibleInstances #-}
module XMonad.Actions.XHints.Hint where

import XMonad.Actions.XHints.State
import XMonad.Actions.XHints.Render (showHint)
import Data.Typeable
import XMonad
import qualified Data.Map as M
import Data.Typeable
import qualified Data.Text as T
import Codec.Binary.UTF8.String (decode)
import Control.Exception.Extensible as E (catch,SomeException(..))
import Data.Maybe (fromMaybe)

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

getState :: forall v . Typeable v => v -> X XHintsState
getState _ = do
  XState {extensibleState = ex} <- get
  let state' = case M.lookup (show xHintsTy) ex of
        Nothing -> (initialValue :: XHintsState)

        Just (Right (StateExtension s)) -> (\(Just x) -> x) $ cast s
        x -> undefined
      ident = typeOf (undefined :: v)
      as = case M.lookup ident (actions state') of
        Nothing -> emptyState
        Just s -> s
      xHintsTy = typeOf (undefined :: XHintsState)
  return $ state'{actions=M.insert ident as (actions state')}
  
runXHint :: forall v . Typeable v => (T.Text -> XHint v) -> X ()
runXHint hint = do
  state <- getState (undefined :: v)
  state' <- case window (actions state M.! ident) of
    Nothing -> do
      sel <- getSelection
      trace (show sel)
      (msg,as) <- runXHints (hint $ T.pack sel) state
      trace (show msg)
      (w,gc) <- showHint msg -- (T.pack $ show msg)
      return $ as{window=Just (w,gc)}
    Just (w,_) -> do
      dpy <- asks display
      liftIO $ destroyWindow dpy w
      return $ (actions state M.! ident){window=Nothing}
  let state'' = state{actions=M.insert ident state' (actions state)}
  modify (\s -> s{extensibleState = M.insert (show xHintsTy) (Right (StateExtension state'')) $ extensibleState s})
  where
    xHintsTy = typeOf (undefined :: XHintsState)
    ident = typeOf (undefined :: v)
