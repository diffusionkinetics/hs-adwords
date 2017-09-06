module AdWords 
  ( module AdWords.Auth
  , module AdWords
  , module Text.XML.Writer
  ) where
  
import AdWords.Auth 

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS

import Network.OAuth.OAuth2.Internal
import Data.Binary (decodeFile, encodeFile)
import Data.Text (Text)
import Text.XML.Writer
import Text.XML
import Control.Monad.RWS
import Network.HTTP.Client (Response(..))

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
  customer <- loadCustomer fcustomer
  creds <- loadCreds fcreds
  evalRWST session creds customer

withCustomer :: ClientCustomerId -> AdWords a -> AdWords a
withCustomer ccid session = do
  oldCustomer <- get
  put (oldCustomer { _clientCustomerID = ccid })
  session <* put oldCustomer

type AWQL = BS.ByteString
type Format = BS.ByteString

header :: Text -> Text -> XML
header customerId devToken =
  "RequestHeader" # do
    "clientCustomerId" ## customerId
    "developerToken"   ## devToken
    "userAgent"        ## "hs-adwords"
    "validateOnly"     ## "false"
    "partialFailure"   ## "false"

api' :: Maybe Text
api' = Just api

api :: Text
api = "https://adwords.google.com/api/adwords/cm/v201708"

type' :: Text -> [(Name, Text)]
type' t = [(Name "xsi:type" Nothing Nothing, t)]

name :: Text -> Name
name str = (Name str api' Nothing)

(#) :: Text -> XML -> XML
a # b = element (Name a api' Nothing) b

(#?) :: Text -> [(Name, Text)] -> XML -> XML
n #? as = elementA (Name n api' Nothing) as 

(##) :: Text -> Text -> XML 
a ## b = a # content b

infixr 7 #
infixr 7 ##

names :: ToXML a => Text -> [a] -> XML
names = many . name

query :: Text -> XML
query str = "query" # "query" ## str

reportXML :: XML -> AdWords (Response BL.ByteString)
reportXML body = reportUrlEncoded url payload
  where payload = [("__rdxml", BL.toStrict (renderLBS (def {rsPretty = False}) doc))]
        url = "https://adwords.google.com/api/adwords/reportdownload/v201705"
        doc = document (name "reportDefinition") body

reportAWQL :: AWQL -> Format -> AdWords (Response BL.ByteString)
reportAWQL queryString format = reportUrlEncoded url payload 
  where payload = [ ("__fmt", format) 
                  , ("__rdquery", queryString) ]
        url = "https://adwords.google.com/api/adwords/reportdownload/v201705"

request :: 
     String 
  -> XML 
  -> AdWords (Response Document)
request serviceUrl body = do
  {-token <- _accessToken <$> get-}
  ccid  <- _clientCustomerID <$> get
  Credentials _ devToken _ <- ask

  let soap' = soap (header ccid devToken) $ body
      req = BL.toStrict . renderLBS settings $ soap'
      settings = def { 
          rsPretty = False 
        , rsNamespaces = [ ("xsi", "http://www.w3.org/2001/XMLSchema-instance") ]
        }

  {-liftIO . BS.putStrLn $ req-}

  fmap (parseLBS_ def) <$> postRequest serviceUrl req
