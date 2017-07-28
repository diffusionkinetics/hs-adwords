{-# LANGUAGE OverloadedStrings #-}
module Main where

import AdWords
import AdWords.Auth
import Network.OAuth.OAuth2

import Control.Monad.RWS

import Data.Map (Map)
import Data.Text (Text)
import Network.OAuth.OAuth2

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as BS

myCreds :: MonadIO m => BS.ByteString -> m (OAuth2Result (Credentials, AccessToken))
myCreds = credentials 
 "560672271820-6isihukhrj7dfpttj5crg2mrc5lu8dm3.apps.googleusercontent.com"
 "RYGhhsVyiG6QU8wKupZKktsw"
 "pqtf_Za64sgh9mOt87sPEA"
 "205-650-7690"

test :: IO ((), Text)
test = withSaved "token" "credentials" $ do
  refresh
  
  res <- request "BudgetService" $ query "select BudgetId"

  {-res <- request "BudgetService" $ do-}
    {-name "get" $ -}
      {-name "serviceSelector" $ do-}
        {-name "fields" $ content "BudgetId"-}
    {-name "mutate" $ -}
      {-name "operations" $ do-}
        {-name "operator" $ content "ADD"-}
        {-name "operand" $ -}
          {-name "name" $ content "onthenh"-}

  {-res <- reportAWQL "select Id from AD_PERFORMANCE_REPORT" "CSV"-}
  liftIO $ print res

testReport :: IO ((), Text)
testReport = withSaved "token" "credentials" $ do
  refresh

  {-res <- reportXML $ do-}
    {-name "selector" $-}
      {-name "fields" $ content "Name"-}
    {-name "reportName" $ content "custom report" -}
    {-name "reportType" $ content "CAMPAIGN_GROUP_PERFORMANCE_REPORT"-}
    {-name "dateRangeType" $ content "LAST_7_DAYS"-}
    {-name "downloadFormat" $ content "CSV"-}

  res <- reportAWQL "select Name from CAMPAIGN_GROUP_PERFORMANCE_REPORT" "CSV"

  liftIO $ print res

main = do
  {-test-}
  testReport
  return ()
