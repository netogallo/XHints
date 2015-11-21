{-# Language OverloadedStrings, ScopedTypeVariables #-}
module XMonad.Actions.XHints.Helpers(newTextHint) where

import Data.Text (Text)
import qualified Data.Text as T
import Graphics.X11.Xlib.Types
import Foreign.C
import XMonad hiding (drawString)
import Graphics.X11.Xft
import Control.Monad (foldM_)
import XMonad.Actions.XHints.State
import Control.Monad.Trans (lift)

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

writeMessages ::  Display -> Window -> GC -> String -> Dimension -> Dimension -> [Text] -> IO ()
writeMessages dpy win gc color w h msgs = do
  let
    cata :: Int -> Text -> IO Int
    cata y msg = do
      drawString dpy win gc color w h (fromIntegral $ pxChar * 1) (fromIntegral y) msg
      return (y + lineHeight)
  foldM_ cata lineHeight msgs
  
showHint :: Display -> Window -> GC -> Either Text Text -> IO ()
showHint dpy win gc message' = do
  trace "about to write"
  resizeWindow dpy win (fromIntegral w) (fromIntegral h)
  writeMessages dpy win gc color (fromIntegral w) (fromIntegral h) msgs
  where
    (message,color) = case message' of
      Right m -> (m,msgGoodColor)
      Left m -> (m,msgErrColor)
    (msgs,w,h) = hintDimensions message

newTextHint :: forall status state  . Typeable status =>
               (Text -> XHint status state (Either Text Text))
               -> (Text -> XHint status state ())
newTextHint hint  selection = do
  XHintsContext{hintWindow=w, graphicsContext = gc, hintDisplay = dpy} <- hintContext
  result <- hint selection
  liftIO $ showHint dpy w gc result
