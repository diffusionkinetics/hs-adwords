{-# LANGUAGE StandaloneDeriving #-}

module AdWords.Auth 
  ( doPostRequest
  , doReportRequest
  , credentials
  , exchangeCodeUrl
  , Credentials (..)
  , refresh
  , AdWords
  ) where

import Data.Binary (Binary)
import GHC.Generics (Generic)

import Network.OAuth.OAuth2
import Network.OAuth.OAuth2
import Network.HTTP.Client.TLS
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Data.Text (Text)
import Data.Monoid
import Control.Monad.RWS

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

tlsManager :: IO Manager
tlsManager = newManager tlsManagerSettings

type AdWords = RWST Credentials Text AccessToken IO

doPostRequest :: 
     String
  -> BS.ByteString 
  -> AdWords (Response BL.ByteString)
doPostRequest url body = do
  req <- liftIO $ parseRequest url
  token <- get
  man <- liftIO tlsManager
  let headers = 
        [ (hUserAgent, "hs-adwords")
        , (hAuthorization, "Bearer " `BS.append` accessToken token)
        , (hContentType, "application/soap+xml")
        ]

      req' = req  { requestHeaders = headers
                  , requestBody = RequestBodyBS body
                  , method = "POST"
                  }
  liftIO $ BS.putStrLn body
  liftIO $ print "-----"
  liftIO $ httpLbs req' man 

tshow :: Show a => a -> Text
tshow = T.pack . show

doReportRequest :: 
     String
  -> BS.ByteString 
  -> AdWords (Response BL.ByteString)
doReportRequest url body = do
  req <- liftIO $ parseRequest url
  token <- get
  man <- liftIO tlsManager
  Credentials _ devToken ccid _ <- ask 
  let headers = 
        [ (hUserAgent, "hs-adwords")
        , (hAuthorization, "Bearer " `BS.append` accessToken token)
        , ("developerToken", BS.pack . show $ devToken)
        , ("clientCustomerId", BS.pack . show $ ccid)
        , ("contentType", "multipart/form-data")
        ]

      req' = req  { requestHeaders = headers
                  , requestBody = RequestBodyBS body
                  , method = "POST"
                  }
  liftIO $ httpLbs req' man 

type ClientId = BS.ByteString
type ClientSectret = BS.ByteString
type DeveloperToken = Text
type CustomerClientId = Text
type ExchangeKey = BS.ByteString

data Credentials = Credentials {
    oauth :: OAuth2
  , developerToken :: DeveloperToken
  , customerClientId :: CustomerClientId
  , refreshToken' :: Maybe BS.ByteString
} deriving (Show, Generic)

instance Binary Credentials
instance Binary OAuth2 
deriving instance Generic OAuth2 

-- refresh token is given only on first key exchange
credentials :: MonadIO m =>
     ClientId 
  -> ClientSectret 
  -> DeveloperToken
  -> CustomerClientId
  -> ExchangeKey
  -> m (OAuth2Result (Credentials, AccessToken))
credentials cliendId clientSecret devToken ccid exchangeKey = liftIO $ do
  let oa = OAuth2 
        cliendId 
        clientSecret 
        authorizeEndpoint 
        accessTokenEntpoint 
        (Just callback)
      accessTokenEntpoint = "https://www.googleapis.com/oauth2/v3/token"
      authorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
      callback = "urn:ietf:wg:oauth:2.0:oob"

  man <- tlsManager 
  res <- fetchAccessToken man oa exchangeKey
  
  return $ fmap (\acc -> (Credentials oa devToken ccid $ refreshToken acc, acc)) res

deriving instance Generic AccessToken
instance Binary AccessToken

exchangeCodeUrl :: BS.ByteString -> BS.ByteString
exchangeCodeUrl cId = 
  "https://accounts.google.com/o/oauth2/auth?client_id=" 
  <> cId 
  <> "&response_type=code&scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fadwords&redirect_uri=urn:ietf:wg:oauth:2.0:oob&access_type=offline&prompt=consent"

refresh :: AdWords ()
refresh = do 
  mb_refToken <- refreshToken' <$> ask

  case mb_refToken of
    Nothing -> tell "\nerr:no refresh token found\n"
    Just refToken -> do
      creds <- oauth <$> ask
      man <- liftIO tlsManager
      mb_new_token <- liftIO $ fetchRefreshToken man creds refToken

      case mb_new_token of
        Left err -> tell $ tshow err
        Right new_token -> do
          tell " refreshed access token "
          put new_token
