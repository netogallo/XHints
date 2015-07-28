{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Actions.XHints.Translate where

import Data.Typeable
import XMonad.Actions.XHints.State
import Language.Bing

data Translator deriving Typeable

translate :: ClientId -> ClientSecret -> BingLanguage -> BingLanguage -> String -> XHint Translator
translate clientId clientSecret from to text = do
  t <- liftIO $ translate clientId clientSecret from to text
  case t of
    Left err -> Left "Error translating text"
    Right trans -> Right trans
  
  
