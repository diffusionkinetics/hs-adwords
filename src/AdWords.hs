module AdWords 
  ( loadCreds
  , loadToken
  , withSaved
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

loadToken :: MonadIO m => FilePath -> m AccessToken
loadToken = liftIO . decodeFile 

saveToken :: MonadIO m => FilePath -> AccessToken -> m ()
saveToken file = liftIO . encodeFile file

loadCreds :: MonadIO m => FilePath -> m Credentials
loadCreds = liftIO . decodeFile 

saveCreds :: MonadIO m => FilePath -> Credentials -> m ()
saveCreds file = liftIO . encodeFile file

-- cached token path -> cached credentials path -> action -> IO (a, Text)
withSaved :: FilePath -> FilePath -> AdWords a -> IO (a, Text)
withSaved ftoken fcreds session = do
  token <- loadToken ftoken
  creds <- loadCreds fcreds
  evalRWST session creds token

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
reportXML body = do
  let payload = "Parameters:"
         <> "\n__rdxml: &lt;?xml version=\"1.0\" encoding=\"UTF-8\"?&gt;\n"
         <> BL.toStrict (renderLBS (def {rsPretty = True}) doc)
      url = "https://adwords.google.com/api/adwords/reportdownload/v201705"
      doc = document (name' "reportDefinition") body

  res <- reportRequest url payload <* liftIO (BS.putStrLn payload)
  liftIO . pprint . parseLBS_ def . responseBody $ res
  return res


reportAWQL :: AWQL -> Format -> AdWords (Response BL.ByteString)
reportAWQL query format = do
  let body = "\nParameters: "
          <> "\n__fmt: " <> format
          <> "\n__rdquery: " <> query
      url = "https://adwords.google.com/api/adwords/reportdownload/v201705"

  res <- reportRequest url body 
  liftIO . pprint . parseLBS_ def . responseBody $ res
  return res
  
request :: 
     String 
  -> XML 
  -> AdWords (Response (Maybe (Map Text Value)))
request serviceName body = do
  token <- get
  Credentials oauth devToken ccid _ <- ask

  let soap' = soap (header ccid devToken) $ body
      req = BL.toStrict . renderLBS (def {rsPretty = True}) $ soap'
      csUrl = "https://adwords.google.com/api/adwords/cm/v201705/" <> serviceName
  res <- postRequest csUrl req
    
  liftIO $ BS.putStrLn req
  liftIO $ print "---"
  liftIO . pprint . parseLBS_ def . responseBody $ res

  return (rval . parseLBS_ def <$> res)
