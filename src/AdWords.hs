module AdWords 
  ( module Text.XML.Writer
  , runAdWords
  , InitialInfo (..)
  , withCustomer
  , AWQL
  , Format
  , header
  , api
  , api'
  , type'
  , name
  , (#) 
  , (#?)
  , (##)
  , names
  , query
  , reportXML
  , reportAWQL
  , request
  ) where
  
import AdWords.Auth 
import AdWords.Service (nameSpace, serviceUrl, Service)
import AdWords.Types

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS

import Control.Monad.RWS
import Data.Text (Text)
import Text.XML.Writer
import Text.XML
import Network.HTTP.Client (Response(..))

withCustomer :: MonadIO m => ClientCustomerId -> AdWords m a -> AdWords m a
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
api = "https://adwords.google.com/api/adwords/cm/v201802"

type' :: Text -> [(Name, Text)]
type' t = [(Name "xsi:type" Nothing Nothing, t)]

name :: Text -> Name
name str = Name ("ns:" <> str) Nothing Nothing

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

reportXML :: MonadIO m => XML -> AdWords m (Response BL.ByteString)
reportXML body = reportUrlEncoded url payload
  where payload = [("__rdxml", BL.toStrict (renderLBS (def {rsPretty = False}) doc))]
        url = "https://adwords.google.com/api/adwords/reportdownload/v201802"
        doc = document (name "reportDefinition") body

reportAWQL :: MonadIO m => AWQL -> Format -> AdWords m (Response BL.ByteString)
reportAWQL queryString format = reportUrlEncoded url payload 
  where payload = [ ("__fmt", format) 
                  , ("__rdquery", queryString) ]
        url = "https://adwords.google.com/api/adwords/reportdownload/v201802"

request :: MonadIO m => 
     Service
  -> XML 
  -> AdWords m (Response Document)
request service body = do
  ccid  <- _clientCustomerID <$> get
  Credentials _ devToken _ <- ask

  let soap' = soap (header ccid devToken) $ body
      req = BL.toStrict . renderLBS settings $ soap'
      settings = def { 
          rsPretty = False
        , rsNamespaces = 
            [ ("xsi", "http://www.w3.org/2001/XMLSchema-instance") 
            , ("ns", nameSpace service)
            ]
        }

  fmap (parseLBS_ def) <$> postRequest (serviceUrl service) req
