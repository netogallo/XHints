{-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}
module XMonad.Actions.XHints.Translate where

import Data.Typeable
import XMonad.Actions.XHints.State
import qualified Language.Bing as B
import Language.Bing (BingLanguage,BingContext,ClientId,ClientSecret,getAccessTokenEither)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.Text (Text)
import Control.Monad.State.Strict

data Translator deriving Typeable

translate :: ClientId -> ClientSecret -> BingLanguage -> BingLanguage -> Text -> XHint BingContext Translator
translate clientId clientSecret from to text = do
  s <- get
  bctx <- case s of
    Nothing ->  liftIO $ getAccessTokenEither clientId clientSecret
    Just ctx -> return $ Right ctx
  case bctx of
    Right bctx' -> do
      res <- liftIO $ B.execBing bctx' (B.translateM text from to)
      case res of
        Left err -> return $ Left "Error translating text"
        Right (trans,bctx'') -> do
          put (Just bctx'')
          return $ Right trans
    Left err -> return $ Left "Error translating text"
  
  
