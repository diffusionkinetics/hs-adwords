module AdWords.Auth where

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

postRequest ::
     String
  -> BS.ByteString
  -> AdWords (Response BL.ByteString)
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

tshow :: Show a => a -> Text
tshow = T.pack . show

text2bs :: Text -> BS.ByteString
text2bs = BS.pack . T.unpack 

bs2text :: BS.ByteString -> Text
bs2text = T.pack . BS.unpack

reportUrlEncoded ::
     String
  -> [(BS.ByteString, BS.ByteString)]
  -> AdWords (Response BL.ByteString)
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

refresh :: AdWords ()
refresh = do
  ccid <- _clientCustomerID <$> get
  Credentials creds _ refToken <- ask

  man <- liftIO tlsManager

  liftIO (fetchRefreshToken man creds refToken) >>=
    either
      (tell . tshow)
      (put . Customer ccid . accessToken)
