{-# LANGUAGE StandaloneDeriving#-}
module AdWords.Auth
  where

import Data.Binary
import GHC.Generics (Generic)

import Network.OAuth.OAuth2.HttpClient
import Network.OAuth.OAuth2.Internal
import Network.HTTP.Client.TLS
import Network.HTTP.Client
import Network.HTTP.Types.Header

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

tlsManager :: IO Manager
tlsManager = newManager tlsManagerSettings

doPostRequest :: 
     Manager
  -> AccessToken
  -> String
  -> BS.ByteString 
  -> IO (OAuth2Result BL.ByteString)
doPostRequest manager token url body = do
  req <- parseRequest url
  authRequest req (updateRequest token (RequestBodyBS body)) manager

updateRequest :: AccessToken -> RequestBody -> Request -> Request
updateRequest token body req = 
  let headers = 
        [ (hUserAgent, "hs-adwords")
        , (hAuthorization, "Bearer " `BS.append` accessToken token)
        , (hContentType, "application/soap+xml")
        ]

   in req { requestHeaders = headers
          , requestBody = body
          , method = "POST"
          }

type ClientId = BS.ByteString
type ClientSectret = BS.ByteString

credentials :: ClientId -> ClientSectret -> OAuth2
credentials cliendId clientSecret = OAuth2 cliendId clientSecret authorizeEndpoint accessTokenEntpoint $ Just callback
  where
    accessTokenEntpoint = "https://www.googleapis.com/oauth2/v3/token"
    authorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
    callback = "urn:ietf:wg:oauth:2.0:oob"

deriving instance Generic AccessToken
instance Binary AccessToken

exchangeCodeUrl cId = print . T.unpack $
  "https://accounts.google.com/o/oauth2/auth?client_id=" 
  `T.append` cId 
  `T.append` "&response_type=code&scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fadwords&redirect_uri=urn:ietf:wg:oauth:2.0:oob&access_type=offline&prompt=consent"
