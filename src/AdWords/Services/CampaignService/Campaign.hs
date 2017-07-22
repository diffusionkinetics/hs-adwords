module AdWords.Services.CampaignService.Campaign where

import qualified AdWords.Services.CampaignService.Enumerations.AdServingOptimizationStatus as AdServingOptimizationStatus
import qualified AdWords.Services.CampaignService.Enumerations.BudgetStatus as BudgetStatus
import qualified AdWords.Services.CampaignService.Enumerations.CampaignStatus as CampaignStatus
import qualified AdWords.Services.CampaignService.Enumerations.DeliveryMethod as DeliveryMethod
import qualified AdWords.Services.CampaignService.Enumerations.Level as Level
import qualified AdWords.Services.CampaignService.Enumerations.ListOperator as ListOperator
import qualified AdWords.Services.CampaignService.Enumerations.NegativeGeoTargetType as NegativeGeoTargetType
import qualified AdWords.Services.CampaignService.Enumerations.PositiveGeoTargetType as PositiveGeoTargetType
import qualified AdWords.Services.CampaignService.Enumerations.RejectionReason as RejectionReason
import qualified AdWords.Services.CampaignService.Enumerations.ServingStatus as ServingStatus
import qualified AdWords.Services.CampaignService.Enumerations.TimeUnit as TimeUnit
import qualified AdWords.Services.CampaignService.Enumerations.UniversalAppBiddingStrategyGoalType as UniversalAppBiddingStrategyGoalType
import qualified AdWords.Services.CampaignService.Enumerations.UniversalAppCampaignAsset as UniversalAppCampaignAsset

{-data Campaing = -}
    {-ID Int -- id-}
  {-| CampaignGroupId Int -- campaignGroupId-}
  {-| Name Text -- name-}
  {-| Status CampaignStatus.Enumeration-}
  {-| ServingStatus ServingStatus.Enumeration-}
  {-| StartDate Text -- startingDate in format YYYYMMDD like this 20171212-}
  {-| EndDate Text -- endDate -}
  {-| Budget Budget-}
  {-| ConversionOptimizerEligibility ConversionOptimizerEligibility-}
  {-| AdServingOptimizationStatus AdServingOptimizationStatus.Enumeration-}
  {-| FrequencyCap FrequencyCap-}

{-data Budget = -}
    {-BudgetId Int -- budgetId-}
  {-| Name Text -- name-}
  {-| Amount Money -}
  {-| DeliveryMethod DeliveryMethod -}
  {-| ReferenceCount Int -- referenceCount-}
  {-| IsExplicitlyShared Bool -- isExplicitlyShared -}
  {-| BudgetStatus BudgetStatus.Enumeration-}
  {-| ConversionOptimizerEligibility-}
    {-AdServingOptimizationStatus-}
    {-FrequencyCap-}
