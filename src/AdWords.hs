module AdWords where

import AdWords.Auth 
import AdWords.Services

import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.OAuth.OAuth2.Internal
import Network.OAuth.OAuth2.HttpClient
import Data.Binary
import Text.XML.Writer
import Text.XML

api' = Just api
api = "https://adwords.google.com/api/adwords/cm/v201705"

my_ccid = "205-650-7690"
my_dev_token = "pqtf_Za64sgh9mOt87sPEA"
my_clientId = "560672271820-6isihukhrj7dfpttj5crg2mrc5lu8dm3.apps.googleusercontent.com"
my_clientSecret = "RYGhhsVyiG6QU8wKupZKktsw"
tokenPoint = "https://www.googleapis.com/oauth2/v3/token"
authPoint = "https://accounts.google.com/o/oauth2/auth"
callback = "urn:ietf:wg:oauth:2.0:oob"

my_credentials = credentials my_clientId my_clientSecret

callCS :: OAuth2 -> Either FilePath AccessToken -> XML -> IO (OAuth2Result BL.ByteString)
callCS credentials mbtoken body = do
  token <- case mbtoken of
    Left path -> decodeFile path :: IO AccessToken
    Right t -> return t
      
  let req = BL.toStrict . renderLBS def . soap (header my_ccid my_dev_token) $ body
      csUrl = "https://adwords.google.com/api/adwords/cm/v201705/CampaignService"
  man <- tlsManager

  res <- doPostRequest man token csUrl req
  case res of 
    Right rval -> return $ Right rval
    Left err -> do
      let Just refresh = refreshToken token
      Right token_new <- fetchRefreshToken man my_credentials refresh
      doPostRequest man token_new csUrl req

header customerId developerToken =
  let header = Name "RequestHeader" api' Nothing
      ccid = Name "clientCustomerId" api' Nothing
      devTok = Name "developerToken" api' Nothing
      usAgent = Name "userAgent" api' Nothing
      valOnl = Name "validateOnly" api' Nothing
      partFail = Name "partialFailure" api' Nothing

   in element header . mapM_ toXML $
        [ element ccid $ content customerId
        , element devTok $ content developerToken
        , element usAgent $ content "mylib"
        , element valOnl $ content "false"
        , element partFail $ content "false"
        ]

