module AdWords 
  ( module AdWords.Auth
  , module AdWords
  , module Text.XML.Writer
  ) where
  
import AdWords.Auth 

import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Network.OAuth.OAuth2.Internal
import Network.OAuth.OAuth2.HttpClient
import Data.Binary (Binary, decodeFile, encodeFile)
import Data.Text (Text)
import Data.Maybe
import Data.Map.Strict (Map)
import Text.XML.Writer
import Text.XML
import Control.Monad.RWS
import Network.HTTP.Client

loadCustomer :: MonadIO m => FilePath -> m Customer
loadCustomer = liftIO . decodeFile 

saveCustomer :: MonadIO m => FilePath -> Customer -> m ()
saveCustomer file = liftIO . encodeFile file

loadCreds :: MonadIO m => FilePath -> m Credentials
loadCreds = liftIO . decodeFile 

saveCreds :: MonadIO m => FilePath -> Credentials -> m ()
saveCreds file = liftIO . encodeFile file

saveExchanged :: OAuth2Result (Credentials, Customer) -> FilePath -> FilePath -> IO ()
saveExchanged res fcreds fcustomer = case res of 
  Left err -> print err
  Right (creds, customer) -> do
    saveCustomer fcustomer customer
    saveCreds fcreds creds
  
-- cached customer path -> cached credentials path -> action -> IO (a, Text)
withSaved :: FilePath -> FilePath -> AdWords a -> IO (a, Text)
withSaved fcustomer fcreds session = do
  state <- loadCustomer fcustomer
  creds <- loadCreds fcreds
  evalRWST session creds state

withCustomer :: ClientCustomerId -> AdWords a -> AdWords a
withCustomer ccid session = do
  oldCustomer <- get
  put (oldCustomer { _clientCustomerID = ccid })
  session <* put oldCustomer

type AWQL = BS.ByteString
type Format = BS.ByteString

header customerId developerToken =
  name "RequestHeader" $ do
    name "clientCustomerId" $ content customerId
    name "developerToken"   $ content developerToken
    name "userAgent"        $ content "hs-adwords"
    name "validateOnly"     $ content "false"
    name "partialFailure"   $ content "false"

api' = Just api
api = "https://adwords.google.com/api/adwords/cm/v201705"

name str = element (Name str api' Nothing)
name' str = (Name str api' Nothing)

names :: ToXML a => Text -> [a] -> XML
names = many . name'

query :: Text -> XML
query = name "query" . name "query" . content

reportXML :: XML -> AdWords (Response BL.ByteString)
reportXML body = reportUrlEncoded url payload
  where payload = [("__rdxml", BL.toStrict (renderLBS (def {rsPretty = False}) doc))]
        url = "https://adwords.google.com/api/adwords/reportdownload/v201705"
        doc = document (name' "reportDefinition") body

reportAWQL :: AWQL -> Format -> AdWords (Response BL.ByteString)
reportAWQL query format = reportUrlEncoded url payload 
  where payload = [ ("__fmt", format) 
                  , ("__rdquery", query) ]
        url = "https://adwords.google.com/api/adwords/reportdownload/v201705"

request :: 
     String 
  -> XML 
  -> AdWords (Response Document)
request serviceName body = do
  token <- _accessToken <$> get
  ccid  <- _clientCustomerID <$> get
  Credentials oauth devToken _ <- ask

  let soap' = soap (header ccid devToken) $ body
      req = BL.toStrict . renderLBS (def {rsPretty = False}) $ soap'
      csUrl = "https://adwords.google.com/api/adwords/cm/v201705/" <> serviceName

  fmap (parseLBS_ def) <$> postRequest csUrl req
