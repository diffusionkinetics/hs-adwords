{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module AdWords.Types where

import Data.Text (Text)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Binary as BI
import qualified Data.Text as T
import Control.Monad.RWS (RWST)

import Data.Binary (Binary)
import GHC.Generics (Generic)
import URI.ByteString
import URI.ByteString.QQ

import Data.Function (on)
import Network.HTTP.Client
import Network.HTTP.Client.TLS 
import Network.OAuth.OAuth2.Internal 
import Network.OAuth.OAuth2.AuthorizationRequest

tlsManager :: IO Manager
tlsManager = newManager tlsManagerSettings

accessTokenEntpoint :: URI
accessTokenEntpoint = [uri|https://www.googleapis.com/oauth2/v3/token|]

authorizeEndpoint :: URI
authorizeEndpoint = [uri|https://accounts.google.com/o/oauth2/auth|]

callback :: Maybe URI
callback = Just [uri|http://localhost:9999/callback|]

type ClientId = Text
type ClientSecret = Text
type DeveloperToken = Text
type ClientCustomerId = Text
type ExchangeKey = BS.ByteString

data InitialInfo = IInfo 
  ClientId 
  ClientSecret
  DeveloperToken
  ClientCustomerId

data Customer = Customer {
    _clientCustomerID :: ClientCustomerId
  , _accessToken :: AccessToken
} deriving (Show, Generic)

data Credentials = Credentials {
    _oauth :: OAuth2
  , _developerToken :: DeveloperToken
  , _refreshToken' :: RefreshToken
} deriving (Show, Generic)

instance Binary OAuth2 where
  put (OAuth2 cid cs _ _ _) = ((*>) `on` BI.put) cid cs
  get = OAuth2 
    <$> BI.get
    <*> BI.get
    <*> pure authorizeEndpoint
    <*> pure accessTokenEntpoint
    <*> pure callback

deriving instance Generic AccessToken
deriving instance Generic RefreshToken
instance Binary AccessToken
instance Binary RefreshToken
instance Binary Credentials
instance Binary Customer

type AdWords = RWST Credentials Text Customer IO
