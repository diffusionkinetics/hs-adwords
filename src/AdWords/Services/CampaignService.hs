{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module AdWords.Services.CampaignService 
  where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Numeric.Natural
import Data.Tagged
import AdWords.Services
import AdWords.AWQL

import Text.XML.Writer
import Text.XML

{-import qualified AdWords.Services.CampaignService.Enumerations.AdServingOptimizationStatus as AdServingOptimizationStatus-}
{-import qualified AdWords.Services.CampaignService.Enumerations.BudgetStatus as BudgetStatus-}
{-import qualified AdWords.Services.CampaignService.Enumerations.CampaignStatus as CampaignStatus-}
{-import qualified AdWords.Services.CampaignService.Enumerations.DeliveryMethod as DeliveryMethod-}
{-import qualified AdWords.Services.CampaignService.Enumerations.Level as Level-}
{-import qualified AdWords.Services.CampaignService.Enumerations.ListOperator as ListOperator-}
{-import qualified AdWords.Services.CampaignService.Enumerations.NegativeGeoTargetType as NegativeGeoTargetType-}
{-import qualified AdWords.Services.CampaignService.Enumerations.PositiveGeoTargetType as PositiveGeoTargetType-}
{-import qualified AdWords.Services.CampaignService.Enumerations.RejectionReason as RejectionReason-}
{-import qualified AdWords.Services.CampaignService.Enumerations.ServingStatus as ServingStatus-}
{-import qualified AdWords.Services.CampaignService.Enumerations.TimeUnit as TimeUnit-}
{-import qualified AdWords.Services.CampaignService.Enumerations.UniversalAppBiddingStrategyGoalType as UniversalAppBiddingStrategyGoalType-}
{-import qualified AdWords.Services.CampaignService.Enumerations.UniversalAppCampaignAsset as UniversalAppCampaignAsset-}

{-import qualified AdWords.Services.CampaignService.Campaign as Campaign-}

{-import AdWords.Services.CampaignService.Campaign (Campaign)-}

{-type Type = Maybe Text-}

{-data UniversalAppCampaignAdsPolicyDecisions =-}
  {-UniversalAppCampaignAdsPolicyDecisions-}
    {-UniversalAppCampaignAsset-}
    {-Text -- asset ID-}
    {-{-[PolicyTopicEntry]-}-}

{-data ListOperations = -}
  {-ListOperations-}
    {-Bool -- clear, indicates that all contents of the list shold be deleted-}
    {-ListOperator-}

{-data FrequencyCap = -}
  {-FrequencyCap-}
    {-Int -- impressions-}
    {-TimeUnit-}
    {-Level-}

{-data ConversionOptimizerEligibility = -}
  {-ConversionOptimizerEligibility-}
    {-Bool -- eligible-}
    {-(NonEmpty RejectionReason)-}

{-data Money = -}
  {-Money-}
    {-Text -- ComparableValue.Type-}
    {-Natural -- microAmount. oneMillion is equivalent to one unit-}

{-data Campaing = -}
  {-Campaign-}
    {-Int -- id-}
    {-Int -- campaignGroupId-}
    {-Text -- name-}
    {-CampaignStatus-}
    {-ServingStatus-}
    {-Text -- startingDate in format YYYYMMDD like this 20171212-}
    {-Text -- endDate -}
    {-Budget-}
    {-(Maybe ConversionOptimizerEligibility) -}

{-data CampaingSettings = -}
    {-DynamicSearchAdsSetting-}
      {-Type -- readonly, ignored if sent-}
      {-Text -- Domain name, to disable set to "-"-}
      {-Text -- LanguageCode-}
      {-Bool -- useSupplierUrlsOnly-}
      {-[Int] -- a list of page feeds-}

  {-| GeoTargetTypeSetting-}
      {-Type -}
      {-PositiveGeoTypeType-}
      {-NegativeGeoTargetType-}

  {-| UniversalAppCampaignSetting-}
      {-Type -}
      {-Text -- appId-}
      {-Text -- description #1-}
      {-Text -- description #2-}
      {-Text -- description #3-}
      {-Text -- description #4-}
      {-[Int] -- youtube media IDs-}
      {-[Int] -- image media IDs-}
      {-UniversalAppBiddingStrategyGoalType-}
      {-ListOperations -- youtube video media IDs operations-}
      {-ListOperations -- image video media IDs operations-}
      {-UniversalAppCampaignAdsPolicyDecisions-}

{-data CampaignService = CampaignService-}

{-instance Service CampaignService where-}
  {-data Method CampaignService where-}
    {-Query :: AWQL -> Metod CampaignService-}

  {-data Rval CampaignService =-}
    {-CampaingPage Int Text Campaing-}

  {-call (Query q) = Tagged empty-}
