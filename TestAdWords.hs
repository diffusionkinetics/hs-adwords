{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHC.IO.Encoding
import AdWords
import AdWords.Types
import AdWords.Details
import AdWords.Service
import AdWords.Auth
import AdWords.Auth.Server 
import Network.OAuth.OAuth2
import Control.Monad.RWS
import Lens.Micro
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Text (Text)
import Network.HTTP.Client
import Network.OAuth.OAuth2.TokenRequest (Errors)
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Data.Text.Prettyprint.Doc

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS

testCreds :: InitialInfo
testCreds = IInfo
 "560672271820-6isihukhrj7dfpttj5crg2mrc5lu8dm3.apps.googleusercontent.com"
 "RYGhhsVyiG6QU8wKupZKktsw"
 "P3qxmMbAvMJuPpolIpsnHQ"
 "288-897-8527"

call :: AdWords () -> IO ((), Text)
call = withSaved "customer" "creds" . withCustomer "149-309-2425" 

addCampaign = call $ do
  refresh
  printResponse CampaignService $ do
    "mutate" #
      "operations" # do
        "operator" ## "ADD"
        "operand" # do
          "name" ## "camp1"
          "budget" #
            "budgetId" ## "1200785698"    
          "advertisingChannelType" ## "SEARCH"
          "biddingStrategyConfiguration" #
            "biddingStrategyId" ## "1573776619"
          

addBudget = call $ do
  refresh
  printResponse BudgetService $ do
    "mutate" # 
      "operations" # do
        "operator" ## "ADD"
        "operand" # do
          "name" ## "first budget"
          "amount" # 
            "microAmount" ## "1000000000"
          "status" ## "ENABLED"
  
addBiddingStrategy = call $ do
  refresh
  printResponse BiddingStrategyService $ do
    "mutate" # 
      "operations" # do
        "operator" ## "ADD"
        "operand" # do
          "name" ## "first bidding strategy"
          "type" ## "TARGET_SPEND"

addAdGroup = call $ do
  refresh
  printResponse AdGroupService $
    "mutate" #
      "operations" # do
        "operator" ## "ADD"
        "operand" # do
          "campaignId" ## "920518723"
          "name" ## "adgroup1"
          
queries = call $ do
  refresh

  {-addCampaignCriterion 920518723 $ -}
    {-Proximity (Right $ CityOnly "Bydgoszcz") KILOMETERS 20-}

  -- addExpandedTextAd 
  --   47620349193 
  --   "hd1" 
  --   "hd2" 
  --   "description" 
  --   "path1" 
  --   "path2"
  --   ["https://time.is"]

  {-pauseAd 47620349193 219215737667-}
  {-enableAd 47620349193 219215737667-}
  {-changeBudget 920518723 1200785698-}
  {-changeBudget 920518723 1201521004-}
  {-changeBidding 920518723 1573776619-}
  {-changeBidding 920518723 1574236878-}
  {-pauseAd 43187312846-}
  {-enableAd 43187312846-}
  -- campaigns
  adGroups
  {-adGroupAds-}
  -- adDetails 219181330005
  {-adRemove 219215737667 47620349193-}
  {-biddingStrategies-}
  {-adStats-}
  {-campaignCriterions-}
  {-budgets-}
  {-campaignFeeds-}
  {-adGroupFeeds-}
  {-feeds-}
  {-campaignGroupPerformanceTarget-}
  -- >>= liftIO . putDoc . vsep . map dshow
  >>= liftIO . putDoc . vsep . map dshow . responseBody
  -- >>= liftIO . pprint . responseBody

serviceCall = call $ do
  refresh
  printResponse BudgetService $ do
    "get" # 
      "selector" # do
        "fields" ## "BudgetId"
    "mutate" #
      "operations" # do
        "operator" ## "ADD"
        "operand" #
          "name" ## "onthenh"

xmlReport = call $ do
  reportXML $ do
    "selector" #
      "fields" ## "Name"
    "reportName" ## "custom report" 
    "reportType" ## "CAMPAIGN_GROUP_PERFORMANCE_REPORT"
    "dateRangeType" ## "LAST_7_DAYS"
    "downloadFormat" ## "CSV"

  >>= liftIO . BL.putStrLn . responseBody

awqlReport = call $ do
  refresh
  reportAWQL 
    "select AdGroupId, Clicks, AveragePosition, Impressions, AverageCost from ADGROUP_PERFORMANCE_REPORT" 
    "CSV"

  >>= liftIO . BL.putStrLn . responseBody

main = do
  setLocaleEncoding utf8

  - awqlReport
  {-serviceCall-}
  {-xmlReport-}
  {-printUrl-}
  {-addBudget-}
  {-addCampaign-}
  {-addBiddingStrategy-}
  {-addAdGroup-}
  queries
