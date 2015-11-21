{-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}
module XMonad.Actions.XHints.Translate where

import Data.Typeable
import XMonad.Actions.XHints.State
import qualified Language.Bing as B
import Language.Bing (BingLanguage,BingContext,ClientId,ClientSecret,getAccessToken,execBing)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.Text (Text)
import Control.Monad.State.Strict
import qualified Data.Text as T
import XMonad.Actions.XHints.Helpers (newTextHint)

data Translator deriving Typeable

translateHint :: ClientId -> ClientSecret -> BingLanguage -> BingLanguage -> Text -> XHint Translator BingContext (Either Text Text)
translateHint clientId clientSecret from to text = do
  s <- get 
  res <- B.runExceptT $ do
    token <- case s of
      Nothing -> getAccessToken clientId clientSecret
      Just ctx -> return ctx

    flip B.runBing token $ do
      trans <- B.translateM text from to
      ctx <- B.getBingCtx
      return (trans,ctx)
  case res of
    Right (trans,token) -> put (Just token) >> return (Right $ T.pack $ show trans)
    _ -> return $ Left "Error translating text"  

translate clientId clientSecret from to = newTextHint $ translateHint clientId clientSecret from to
