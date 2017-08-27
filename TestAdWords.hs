{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHC.IO.Encoding
import AdWords
import AdWords.Auth
import AdWords.Details
import Network.OAuth.OAuth2

import Control.Monad.RWS

import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Text (Text)
import Network.HTTP.Client

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS

myCreds :: MonadIO m => BS.ByteString -> m (OAuth2Result (Credentials, Customer))
myCreds = credentials 
 "560672271820-6isihukhrj7dfpttj5crg2mrc5lu8dm3.apps.googleusercontent.com"
 "RYGhhsVyiG6QU8wKupZKktsw"
 "FvCyP0BKDLaAzIIwy3aOwA"
 "205-650-7690"

printUrl = BS.putStrLn $ exchangeCodeUrl "560672271820-6isihukhrj7dfpttj5crg2mrc5lu8dm3.apps.googleusercontent.com"

predefinedQueries :: IO ((), Text)
predefinedQueries = withSaved "customer" "creds" $ do
  withCustomer "415-895-2168" $ do
    refresh
    {-campaigns-}
    {-adGroups-}
    {-adGroupAds-}
    adStats
    {-budgets-}
    {-campaignFeeds-}
    {-adGroupFeeds-}
    {-feeds-}
    {-campaignGroupPerformanceTarget-}
    >>= either 
      (liftIO . print)
      (traverse_ $ liftIO . putStrLn . show)
    {->>= liftIO . BL.putStrLn-}

testServiceCall = withSaved "customer" "creds" . 
  withCustomer "415-895-2168" $ do
    res <- request "BudgetService" $ do
      name "get" $ 
        name "serviceSelector" $ do
          name "fields" $ content "BudgetId"
      name "mutate" $ 
        name "operations" $ do
          name "operator" $ content "ADD"
          name "operand" $ 
            name "name" $ content "onthenh"
    liftIO $ print res

testAwqlQuery = withSaved "customer" "creds" . 
  withCustomer "415-895-2168" $ do
    refresh
    reportAWQL 
      "select Impressions, CreativeQualityScore, Clicks, AveragePosition from KEYWORDS_PERFORMANCE_REPORT"
      "CSV"
    >>= liftIO . BL.putStrLn . responseBody

xmlReport :: IO ((), Text)
xmlReport = withSaved "customer" "creds" $ do
  refresh
  withCustomer "415-895-2168" $ reportXML $ do
    name "selector" $
      name "fields" $ content "Name"
    name "reportName" $ content "custom report" 
    name "reportType" $ content "CAMPAIGN_GROUP_PERFORMANCE_REPORT"
    name "dateRangeType" $ content "LAST_7_DAYS"
    name "downloadFormat" $ content "CSV"

  >>= liftIO . BL.putStrLn . responseBody

clickPerformanceExample = withSaved "customer" "creds" .
  withCustomer "415-895-2168" $ do
    refresh
    reportAWQL 
      "select Clicks from ADGROUP_PERFORMANCE_REPORT" 
      "CSV"

    >>= liftIO . BL.putStrLn . responseBody

awqlReport = withSaved "customer" "creds" . 
  withCustomer "415-895-2168" $ do
    refresh
    reportAWQL 
      "select AdGroupId, Clicks, AveragePosition, Impressions, AverageCost from ADGROUP_PERFORMANCE_REPORT" 
      "CSV"

    >>= liftIO . BL.putStrLn . responseBody

main = do
  setLocaleEncoding utf8
  {-testAwqlQuery-}
  {-xmlReport-}
  {-awqlReport-}
  {-printUrl-}
  predefinedQueries
  {-clickPerformanceExample-}
