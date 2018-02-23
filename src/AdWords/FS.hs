module AdWords.FS where

import AdWords.Types
import Data.Binary (decodeFile, encodeFile)
import Network.OAuth.OAuth2.Internal (OAuth2Result)
import Data.Text
import Control.Monad ((<=<))

saveCredentials :: MonadIO m => FilePath -> (Credentials, Customer) -> m ()
saveCredentials path = liftIO . encodeFile path

loadCredentials :: MonadIO m => FilePath -> m (Credentials, Customer)
loadCredentials = liftIO . decodeFile 

saveExchanged :: (MonadIO m, Show e) => 
   FilePath -> OAuth2Result e (Credentials, Customer) -> m ()
saveExchanged filePath = either
  (liftIO . print)
  (saveCredentials filePath)
  
withSaved :: MonadIO m => FilePath -> AdWords m a -> m (a, Customer, Text)
withSaved filePath session = go filePath where
  go = uncurry (runAdWords session) <=< loadCredentials 
