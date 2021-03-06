module AdWords.Auth.Server
  ( authServer
  , authViaBrowser
  , initCredentials
  )
  where

import URI.ByteString
import URI.ByteString.QQ
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Web.Browser (openBrowser)
import Web.Scotty
import Network.OAuth.OAuth2
import Data.Bool (bool)
import Data.Text (Text)
import Data.Monoid ((<>))

import qualified Data.Text as T
import qualified Network.OAuth.OAuth2.TokenRequest as OTR

import AdWords.Types
import AdWords.FS(saveExchanged)

getExchangeCode :: InitialInfo -> IO Bool
getExchangeCode = openBrowser . exchangeCodeUrl

exchangeCodeUrl :: InitialInfo -> String
exchangeCodeUrl (IInfo cid _ _ _) =
  "https://accounts.google.com/o/oauth2/auth?client_id="
  <> T.unpack cid
  <> "&response_type=code&scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fadwords&redirect_uri=http://localhost:9999/callback&access_type=offline&prompt=consent"

authServer :: FilePath -> InitialInfo -> IO ()
authServer file = scotty 9999 . get "/callback" . callbackH file

authViaBrowser :: FilePath -> InitialInfo -> IO ()
authViaBrowser file info = getExchangeCode info >>= bool
  (putStrLn "error: failed to open system browser")
  (authServer file info)

callbackH :: FilePath -> InitialInfo -> ActionM ()
callbackH file info = do
    code <- param "code"
    bool (liftIO $
            initCredentials callback info (ExchangeToken code) >>=
            saveExchanged file)
         (liftIO $ print "error: invalid authorization code")
         (null $ T.unpack code)

initCredentials :: MonadIO m =>
     Maybe URI
  -> InitialInfo
  -> ExchangeToken
  -> m (OAuth2Result OTR.Errors (Credentials, Customer))
initCredentials mcb (IInfo cliendId clientSecret devToken ccid) = liftIO . go
  where
    oa = OAuth2
            cliendId
            clientSecret
            authorizeEndpoint
            accessTokenEntpoint
            mcb
    getRefreshToken :: OAuth2Result OTR.Errors OAuth2Token
                    -> OAuth2Result OTR.Errors RefreshToken
    getRefreshToken (Left err) = Left err
    getRefreshToken (Right (OAuth2Token _ mbreft _ _ _)) = case mbreft of
      Just reftoken -> Right reftoken
      Nothing -> Left $ OAuth2Error
        (Left "no RefreshToken received")
        (Just "no RefreshToken received")
        Nothing

    go xchanget = do
      man <- tlsManager
      res <- fetchAccessToken man oa xchanget
      return $ (,)
          <$> (Credentials oa devToken <$> getRefreshToken res)
          <*> (Customer ccid . accessToken <$> res)
