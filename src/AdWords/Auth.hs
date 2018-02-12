{-# LANGUAGE StandaloneDeriving #-}

module AdWords.Auth
  {-( postRequest-}
  {-, reportUrlEncoded-}
  {-, credentials-}
  {-, exchangeCodeUrl-}
  {-, Credentials (..)-}
  {-, refresh-}
  {-, AdWords-}
  {-, Customer-}
  {-) -}
  where

import Data.Binary (Binary)
import URI.ByteString
import GHC.Generics (Generic)
import Blaze.ByteString.Builder

import Network.OAuth.OAuth2
import Network.OAuth.OAuth2.TokenRequest (Errors(..))
import Network.HTTP.Client.TLS 
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Data.Text (Text)
import Data.String (fromString)
import Data.Monoid ((<>))
import Control.Monad.RWS 
import Lens.Micro ((<&>))

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Binary as BI
import qualified Data.Text as T

tlsManager :: IO Manager
tlsManager = newManager tlsManagerSettings

type AdWords = RWST Credentials Text Customer IO

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

type ClientId = Text
type ClientSectret = Text
type DeveloperToken = Text
type ClientCustomerId = Text
type ExchangeKey = BS.ByteString

data Customer = Customer {
    _clientCustomerID :: ClientCustomerId
  , _accessToken :: AccessToken
} deriving (Show, Generic)

data Credentials = Credentials {
    _oauth :: OAuth2
  , _developerToken :: DeveloperToken
  , _refreshToken' :: RefreshToken
} deriving (Show, Generic)

-- refresh token is given only on first key exchange
credentials :: MonadIO m =>
     ClientId
  -> ClientSectret
  -> DeveloperToken
  -> ClientCustomerId
  -> ExchangeToken
  -> m (OAuth2Result Errors (Credentials, Customer))
credentials cliendId clientSecret devToken ccid xchanget = liftIO $ go
  where 
    oa = OAuth2
            cliendId
            clientSecret
            authorizeEndpoint
            accessTokenEntpoint
            callback
    accessTokenEntpoint = URI
      (Scheme "https")
      (Just $ Authority Nothing (Host "www.googleapis.com") Nothing)
      "/oauth2/v3/token"
      (Query [])
      Nothing
    authorizeEndpoint = URI 
      (Scheme "https")
      (Just $ Authority Nothing (Host "accounts.google.com") Nothing)
      "/o/oauth2/auth"
      (Query [])
      Nothing 
    callback = Nothing
      -- Just "urn:ietf:wg:oauth:2.0:oob"

    getRefreshToken :: OAuth2Result Errors OAuth2Token -> OAuth2Result Errors RefreshToken
    getRefreshToken (Right (OAuth2Token _ mbreft _ _ _)) = case mbreft of
      Just reftoken -> Right reftoken
      Nothing -> Left $ OAuth2Error 
        (Left "no RefreshToken received") 
        (Just "no RefreshToken received")
        Nothing

    go = do 
      man <- tlsManager 
      res <- fetchAccessToken man oa xchanget
      return $ (,) 
          <$> (Credentials oa devToken <$> getRefreshToken res)
          <*> (Customer ccid . accessToken <$> res)


deriving instance Generic AccessToken
deriving instance Generic RefreshToken
instance Binary AccessToken
instance Binary RefreshToken

exchangeCodeUrl :: BS.ByteString -> BS.ByteString
exchangeCodeUrl cId =
  "https://accounts.google.com/o/oauth2/auth?client_id="
  <> cId
  <> "&response_type=code&scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fadwords&redirect_uri=urn:ietf:wg:oauth:2.0:oob&access_type=offline&prompt=consent"

refresh :: AdWords ()
refresh = do
  ccid <- _clientCustomerID <$> get
  Credentials creds _ refToken <- ask

  man <- liftIO tlsManager

  liftIO (fetchRefreshToken man creds refToken) >>=
    either
      (tell . tshow)
      (put . Customer ccid . accessToken)
