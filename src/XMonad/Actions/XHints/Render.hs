{-# Language OverloadedStrings #-}
module XMonad.Actions.XHints.Render where

import XMonad hiding (drawString)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (foldM_)
import Foreign.C
import Graphics.X11.Xlib.Types
import qualified Data.Text.Foreign as TF
import Graphics.X11.Xft
import qualified Data.ByteString as BS
import Codec.Binary.UTF8.String

lineHeight = 20
lineChars = 50
pxChar = 5

isSpc :: Char -> Bool
isSpc = (== ' ')

takeLine :: Text -> Maybe (Text,Text)
takeLine txt
  | T.empty == txt = Nothing
  | fstLen > lineChars = Just $ T.splitAt (lineChars) txt
  | otherwise = consume "" txt
    
  where
    fstLen = T.length (T.takeWhile (not . isSpc) txt)
    consume acc rest
      | rest == T.empty = Just (acc,rest)
      | otherwise =
        let next = T.takeWhile (not . isSpc) rest
            rest' = T.drop (T.length next) rest
        in if T.length acc + T.length next < lineChars then
             consume (T.concat [acc,next,T.take 1 rest']) $ T.drop 1 rest'
           else
             Just (acc,rest)

takeLines :: Text -> [Text]
takeLines txt = case takeLine txt of
  Just (line,rest) -> line : takeLines rest
  Nothing -> []

hintDimensions :: Text -> ([Text],Int,Int)
hintDimensions msg = (txt, lineChars * pxChar, 1 + length txt * lineHeight)
  where
    txt = takeLines msg

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

hintWindow :: Int -> Int -> X (Window,GC)
hintWindow w h = do
  dpy <- asks display
  let win = defaultRootWindow dpy
      blk = blackPixel dpy $ defaultScreen dpy
      wht = whitePixel dpy $ defaultScreen dpy
      scn = defaultScreenOfDisplay dpy
  (_,_,_,_,_,x,y,_) <- liftIO $ queryPointer dpy win
  nw <- liftIO $ do
    -- nw <- mkUnmanagedWindow dpy scn win (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
    nw' <- createSimpleWindow dpy win (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) 1 blk wht
    mapWindow dpy nw'
    return nw'
--  manage nw
  gc <- liftIO $ createGC dpy nw
  return (nw,gc)

drawString :: Display -> Drawable -> GC -> String -> Dimension -> Dimension -> Position -> Position -> T.Text -> IO ()
drawString display d gc colorStr w h x y str = do
  px <- createPixmap display d w h (defaultDepth display 0)
  bm <- xftDrawCreateBitmap display px
  font <- xftFontOpen display scn "Liberation Serif"
  -- color <- withXftColorName display visual bitmap "black" return
  let putMsg drw = withXftColorName display visual bitmap colorStr $ \color -> xftDrawString drw color font x y $ (T.unpack str)
  withXftDraw display d visual bitmap putMsg
  
  where
    scn = defaultScreenOfDisplay display
    visual = defaultVisual display 0
    bitmap = defaultColormap display 0

msgGoodColor :: String
msgGoodColor = "black"

msgErrColor :: String
msgErrColor = "red"

writeMessages :: Window -> GC -> String -> Dimension -> Dimension -> [Text] -> X ()
writeMessages win gc color w h msgs = do
  dpy <- asks display
  let
    cata :: Int -> Text -> IO Int
    cata y msg = do
      drawString dpy win gc color w h (fromIntegral $ pxChar * 1) (fromIntegral y) msg
      return (y + lineHeight)
  liftIO $ foldM_ cata lineHeight msgs
  
showHint :: Either Text Text -> X (Window,GC)
showHint message' = do
  (win,gc) <- hintWindow w h
  trace "about to write"
  writeMessages win gc color (fromIntegral w) (fromIntegral h) msgs
  return (win,gc)
  where
    (message,color) = case message' of
      Right m -> (m,msgGoodColor)
      Left m -> (m,msgErrColor)
    (msgs,w,h) = hintDimensions message
