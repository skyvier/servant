{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.Client.Core.RequestSpec (spec) where


import           Prelude ()
import           Prelude.Compat
import           Control.Monad
import           Data.List (isInfixOf)
import           Servant.Client.Core.Request
import           Servant.Client.Core.Auth
import           Test.Hspec

newtype DataWithRequest = DataWithRequest (RequestF RequestBody ())
  deriving Show

spec :: Spec
spec = do
  describe "Request" $ do
    describe "show" $ do
      it "has parenthesis correctly positioned" $ do
        let d = DataWithRequest (void defaultRequest)
        show d `shouldBe` "DataWithRequest (Request {requestPath = ()\
                                                  \, requestQueryString = fromList []\
                                                  \, requestBody = Nothing\
                                                  \, requestAccept = fromList []\
                                                  \, requestHeaders = fromList []\
                                                  \, requestHttpVersion = HTTP/1.1\
                                                  \, requestMethod = \"GET\"})"
      it "redacts the authorization header" $ do
        let request = void $ defaultRequest { requestHeaders = pure $ PublicHeader ("authorization", "secret") }
        isInfixOf "secret" (show request) `shouldBe` False
      it "redacts a sensitive header" $ do
        let (secretHeaderValue :: String) = "secret"
            request =
              void $ addSensitiveHeader "X-Sensitive-Token" secretHeaderValue defaultRequest
        isInfixOf "secret" (show request) `shouldBe` False
      it "does not redact a public headers" $ do
        let (headerValue :: String) = "not-a-secret"
            request =
              void $ addHeader "Not-Sensitive-Token" headerValue defaultRequest
        isInfixOf "not-a-secret" (show request) `shouldBe` True
