{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module AdWords.Types 
  ( tlsManager
  , accessTokenEntpoint
  , authorizeEndpoint
  , callback
  , ClientId
  , ClientSecret
  , ClientCustomerId
  , DeveloperToken
  , ExchangeKey
  , InitialInfo (..)
  , Customer (..)
  , Credentials (..)
  , AdWords
  , runAdWords
  , MonadIO
  , liftIO
  , tshow
  ) where

import Data.Text (Text)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Binary as BI
import qualified Data.Text as T
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import URI.ByteString
import URI.ByteString.QQ

import Data.Function (on)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.OAuth.OAuth2.Internal 

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

tshow :: Show a => a -> Text
tshow = T.pack . show

deriving instance Generic AccessToken
deriving instance Generic RefreshToken
instance Binary AccessToken
instance Binary RefreshToken
instance Binary Credentials
instance Binary Customer

type AdWords m = RWST Credentials Text Customer m

runAdWords :: MonadIO m => 
     AdWords m a 
  -> Credentials 
  -> Customer
  -> m (a, Customer, Text)
runAdWords r = runRWST r
