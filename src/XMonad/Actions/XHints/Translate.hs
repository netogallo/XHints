{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Actions.XHints.Translate where

import Data.Typeable
import XMonad.Actions.XHints.State
import qualified Language.Bing as B
import Language.Bing (BingLanguage,ClientId,ClientSecret)
import Control.Monad.IO.Class (MonadIO,liftIO)

data Translator deriving Typeable

translate :: ClientId -> ClientSecret -> BingLanguage -> BingLanguage -> String -> XHint Translator
translate clientId clientSecret from to text = do
  t <- liftIO $ B.translate clientId clientSecret text from to
  return $ case t of
    Left err -> Left "Error translating text"
    Right trans -> Right trans
  
  
