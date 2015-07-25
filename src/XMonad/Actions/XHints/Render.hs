{-# Language OverloadedStrings #-}
module XMonad.Actions.XHints.Render where

import XMonad
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (foldM_)

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
    consume acc rest =
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
hintDimensions msg = (txt, lineChars * pxChar, length txt * lineHeight)
  where
    txt = takeLines msg

hintWindow :: Int -> Int -> X (Window,GC)
hintWindow w h = do
  dpy <- asks display
  let win = defaultRootWindow dpy
      blk = blackPixel dpy $ defaultScreen dpy
      wht = whitePixel dpy $ defaultScreen dpy
  (_,_,_,_,_,x,y,_) <- liftIO $ queryPointer dpy win
  nw <- liftIO $ do
    nw <- createSimpleWindow dpy win (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) 1 blk wht
    mapWindow dpy nw
    return nw
  manage nw
  gc <- liftIO $ createGC dpy nw
  return (nw,gc)
  
writeMessages win gc msgs = do
  dpy <- asks display
  let
    cata :: Int -> Text -> IO Int
    cata y msg = do
      drawString dpy win gc 0 (fromIntegral y) (T.unpack msg)
      return (y + lineHeight)
  liftIO $ foldM_ cata 0 msgs
  
showHint :: Text -> X (Window,GC)
showHint message = do
  (win,gc) <- hintWindow w h
  writeMessages win gc msgs
  return (win,gc)
  where (msgs,w,h) = hintDimensions message
