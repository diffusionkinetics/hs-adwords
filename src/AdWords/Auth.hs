module AdWords.Auth 
  ( postRequest
  , reportUrlEncoded
  , refresh
  , authorize
  , defaultIInfo
  ) where

import Network.OAuth.OAuth2
import Network.OAuth.OAuth2.TokenRequest (Errors(..))
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Client.TLS 
import Data.Text (Text)
import Data.Monoid ((<>))
import Control.Monad.RWS 
import Lens.Micro ((<&>))

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import AdWords.Types
import AdWords.Auth.Server (authorize)

defaultIInfo :: DeveloperToken -> ClientCustomerId -> InitialInfo
defaultIInfo = IInfo 
 "560672271820-6isihukhrj7dfpttj5crg2mrc5lu8dm3.apps.googleusercontent.com"
 "RYGhhsVyiG6QU8wKupZKktsw"

postRequest :: MonadIO m => 
     String
  -> BS.ByteString
  -> AdWords m (Response BL.ByteString)
postRequest url body = do
  req <- liftIO $ parseRequest url
  token <- _accessToken <$> get
  man <- liftIO tlsManager
  let headers =
        [ (hUserAgent, "hs-adwords")
        , (hAuthorization, "Bearer " <> text2bs (atoken token))
        , (hContentType, "application/soap+xml")
        ]

      req' = req  { requestHeaders = headers
                  , requestBody = RequestBodyBS body
                  , method = "POST"
                  }
  liftIO $ httpLbs req' man

text2bs :: Text -> BS.ByteString
text2bs = BS.pack . T.unpack 

reportUrlEncoded :: MonadIO m => 
     String
  -> [(BS.ByteString, BS.ByteString)]
  -> AdWords m (Response BL.ByteString)
reportUrlEncoded url body = do
  req <- liftIO $ parseRequest url
  token <- _accessToken <$> get
  man <- liftIO tlsManager
  devToken <- _developerToken <$> ask
  ccid <- _clientCustomerID <$> get
  let headers =
        [ (hUserAgent, "hs-adwords")
        , (hAuthorization, "Bearer " <> text2bs (atoken token))
        , ("developerToken", BS.pack $ T.unpack devToken)
        , ("clientCustomerId", BS.pack $ T.unpack ccid)
        ]

      req' = req  { requestHeaders = headers }

  liftIO $ httpLbs (urlEncodedBody body req') man

refresh :: MonadIO m => AdWords m ()
refresh = do
  ccid <- _clientCustomerID <$> get
  Credentials creds _ refToken <- ask

  man <- liftIO tlsManager

  liftIO (fetchRefreshToken man creds refToken) >>=
    either
      (tell . tshow)
      (put . Customer ccid . accessToken)
