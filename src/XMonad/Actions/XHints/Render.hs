{-# Language OverloadedStrings #-}
module XMonad.Actions.XHints.Render where

import XMonad hiding (drawString)
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C
import Graphics.X11.Xlib.Types
import qualified Data.Text.Foreign as TF
import qualified Data.ByteString as BS
import Codec.Binary.UTF8.String

mkUnmanagedWindow :: Display -> Screen -> Window -> Position
                  -> Position -> Dimension -> Dimension -> IO Window
mkUnmanagedWindow d s rw x y w h = do
  let visual = defaultVisualOfScreen s
      attrmask = cWOverrideRedirect
  allocaSetWindowAttributes $
         \attributes -> do
           set_override_redirect attributes True
           createWindow d rw x y w h 0 (defaultDepthOfScreen s)
                        inputOutput visual attrmask attributes

newHintWindow :: Display  -> IO (Window,GC)
newHintWindow dpy = do
  let win = defaultRootWindow dpy
      blk = blackPixel dpy $ defaultScreen dpy
      wht = whitePixel dpy $ defaultScreen dpy
      scn = defaultScreenOfDisplay dpy
  (_,_,_,_,_,x,y,_) <- queryPointer dpy win
  nw <- createSimpleWindow dpy win (fromIntegral x) (fromIntegral y) 2 2 1 blk wht
  mapWindow dpy nw
  gc <- createGC dpy nw
  return (nw,gc)
