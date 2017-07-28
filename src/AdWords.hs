module AdWords 
  ( loadCreds
  , loadCustomer
  , saveCustomer
  , saveCreds
  , withSaved
  , withCustomer
  , reportAWQL
  , reportXML
  , query
  , name
  , element
  , content
  , empty
  , request
  -- reexports
  , refresh
  , credentials
  , Credentials 
  , exchangeCodeUrl
  , AdWords
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

query :: Text -> XML
query = name "query" . name "query" . content

data Value = String Text | Object (Map Text Value) | List [Value] deriving Show

rval :: Document -> Maybe (Map Text Value)
rval (Document _ root _) = fmap goElem . listToMaybe . findBody $ root
  where
    findBody :: Element -> [Element]
    findBody e@(Element (Name name _ _) _ ns) 
      | T.isInfixOf "Body" name = [e]
      | otherwise = concat . fmap findBody . go $ ns

    go (NodeElement el : xs) = el : go xs
    go (_ : xs) = go xs
    go [] = []

    goElem :: Element -> Map Text Value
    goElem (Element (Name name _ _) _ ns) 
      | length ns == 1 = Map.singleton name . goNode . head $ ns
      | length ns >= 2 = Map.singleton name $ List (goNode <$> ns)
      | otherwise = Map.empty

    goNode :: Node -> Value
    goNode n = case n of 
      NodeElement el -> Object $ goElem el
      NodeContent val -> String val

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
  -> AdWords (Response (Maybe (Map Text Value)))
request serviceName body = do
  token <- _accessToken <$> get
  ccid  <- _clientCustomerID <$> get
  Credentials oauth devToken _ <- ask

  let soap' = soap (header ccid devToken) $ body
      req = BL.toStrict . renderLBS (def {rsPretty = True}) $ soap'
      csUrl = "https://adwords.google.com/api/adwords/cm/v201705/" <> serviceName
  res <- postRequest csUrl req
    
  liftIO $ BS.putStrLn req
  liftIO $ print "---"
  liftIO . pprint . parseLBS_ def . responseBody $ res

  return (rval . parseLBS_ def <$> res)
