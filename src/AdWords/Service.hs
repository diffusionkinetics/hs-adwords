module AdWords.Service where

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

data Service = 
    AccountLabelService
  | AdCustomizerFeedService
  | AdGroupAdService
  | AdGroupBidModifierService
  | AdGroupCriterionService
  | AdGroupExtensionService
  | AdGroupFeedService
  | AdGroupService
  | AdParamService
  | AdwordsUserListService
  | BatchJobService
  | BiddingStrategyService
  | BudgetOrderService
  | BudgetService
  | CampaignBidModifierService
  | CampaignCriterionService
  | CampaignExtensionSettingService
  | CampaignFeedService
  | CampaignService
  | CampaignGroupPerformanceTargetService
  | CampaignGroupService
  | CampaignSharedSetService
  | ConstantDataService
  | ConversionTrackerService
  | CustomerExtensionSettingService
  | CustomerFeedService
  | CustomerService
  | CustomerSyncService
  | DataService
  | DraftAsyncErrorService
  | DraftService
  | FeedItemService
  | FeedMappingService
  | FeedService
  | LabelService
  | LocationCriterion
  | ManagedCustomerService
  | MediaService
  | OfflineCallConversionFeedService
  | OfflineConversionFeedService
  | OfflineDataUploadService
  | ReportDefinitionService
  | SharedCriterionService
  | SharedSetService
  | TargetingIdeaService
  | TrafficEstimatorService
  | TrialAsyncErrorService
  | TrialService


serviceUrl :: Service -> String
serviceUrl x = T.unpack $ case x of
  AccountLabelService
    -> "https://adwords.google.com/api/adwords/mcm/" <> version <> "/AccountLabelService"
  AdCustomizerFeedService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/AdCustomizerFeedService"
  AdGroupAdService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/AdGroupAdService"
  AdGroupBidModifierService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/AdGroupBidModifierService"
  AdGroupCriterionService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/AdGroupCriterionService"
  AdGroupExtensionService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/AdGroupExtensionSettingService"
  AdGroupFeedService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/AdGroupFeedService"
  AdGroupService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/AdGroupService"
  AdParamService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/AdParamService"
  AdwordsUserListService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/AdParamService"
  BatchJobService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/BatchJobService"
  BiddingStrategyService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/BiddingStrategyService"
  BudgetOrderService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/BiddingStrategyService"
  BudgetService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/BudgetService"
  CampaignBidModifierService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/CampaignBidModifierService"
  CampaignCriterionService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/CampaignCriterionService"
  CampaignExtensionSettingService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/CampaignExtensionSettingService"
  CampaignFeedService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/CampaignFeedService"
  CampaignGroupPerformanceTargetService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/CampaignGroupPerformanceTargetService"
  CampaignGroupService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/CampaignGroupService"
  CampaignService 
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/CampaignService"
  CampaignSharedSetService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/CampaignSharedSetService"
  ConstantDataService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/ConstantDataService"
  ConversionTrackerService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/ConversionTrackerService"
  CustomerExtensionSettingService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/CustomerExtensionSettingService"
  CustomerFeedService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/CustomerFeedService"
  CustomerService
    -> "https://adwords.google.com/api/adwords/mcm/" <> version <> "/CustomerService"
  CustomerSyncService
    -> "https://adwords.google.com/api/adwords/ch/" <> version <> "/CustomerSyncService"
  DataService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/DataService"
  DraftAsyncErrorService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/DraftAsyncErrorService"
  DraftService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/DraftService"
  FeedItemService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/FeedItemService"
  FeedMappingService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/FeedMappingService"
  FeedService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/FeedService"
  LabelService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/LabelService"
  LocationCriterion
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/LocationCriterionService"
  ManagedCustomerService
    -> "https://adwords.google.com/api/adwords/mcm/" <> version <> "/ManagedCustomerService"
  MediaService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/MediaService"
  OfflineCallConversionFeedService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/OfflineCallConversionFeedService"
  OfflineDataUploadService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/OfflineConversionFeedService"
  ReportDefinitionService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/ReportDefinitionService"
  SharedCriterionService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/SharedCriterionService"
  SharedSetService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/SharedSetService"
  TargetingIdeaService
    -> "https://adwords.google.com/api/adwords/o/" <> version <> "/TargetingIdeaService"
  TrafficEstimatorService
    -> "https://adwords.google.com/api/adwords/o/" <> version <> "/TrafficEstimatorService"
  TrialAsyncErrorService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/TrialAsyncErrorService"
  TrialService
    -> "https://adwords.google.com/api/adwords/cm/" <> version <> "/TrialService"

version :: Text
version = "v201708"

cm :: Text
cm = "https://adwords.google.com/api/adwords/cm/" <> version

mcm :: Text
mcm = "https://adwords.google.com/api/adwords/mcm/" <> version

rm :: Text 
rm = "https://adwords.google.com/api/adwords/rm/" <> version

billing :: Text
billing = "https://adwords.google.com/api/adwords/billing/" <> version

ch :: Text
ch = "https://adwords.google.com/api/adwords/ch/" <> version

o :: Text
o = "https://adwords.google.com/api/adwords/o/" <> version

nameSpace :: Service -> Text
nameSpace x = case x of
  AccountLabelService
    -> mcm
  AdCustomizerFeedService
    -> cm
  AdGroupAdService
    -> cm
  AdGroupBidModifierService
    -> cm
  AdGroupCriterionService
    -> cm
  AdGroupExtensionService
    -> cm
  AdGroupFeedService
    -> cm
  AdGroupService
    -> cm 
  AdParamService
    -> cm
  AdwordsUserListService
    -> rm
  BatchJobService
    -> cm
  BiddingStrategyService
    -> cm 
  BudgetOrderService
    -> billing
  BudgetService
    -> cm
  CampaignBidModifierService
    -> cm
  CampaignCriterionService
    -> cm
  CampaignExtensionSettingService
    -> cm 
  CampaignFeedService
    -> cm
  CampaignGroupPerformanceTargetService
    -> cm
  CampaignGroupService
    -> cm
  CampaignService
    -> cm
  CampaignSharedSetService
    -> cm
  ConstantDataService
    -> cm
  ConversionTrackerService
    -> cm
  CustomerExtensionSettingService
    -> cm
  CustomerFeedService
    -> cm
  CustomerService
    -> mcm
  CustomerSyncService
    -> ch
  DataService
    -> cm
  DraftAsyncErrorService
    -> cm
  DraftService
    -> cm
  FeedItemService
    -> cm
  FeedMappingService
    -> cm
  FeedService
    -> cm
  LabelService
    -> cm
  LocationCriterion
    -> cm
  ManagedCustomerService
    -> mcm
  MediaService
    -> cm
  OfflineCallConversionFeedService
    -> cm
  OfflineConversionFeedService
    -> cm
  OfflineDataUploadService
    -> rm
  ReportDefinitionService
    -> cm
  SharedCriterionService
    -> cm
  SharedSetService
    -> cm
  TargetingIdeaService
    -> o
  TrafficEstimatorService
    -> o
  TrialAsyncErrorService
    -> cm
  TrialService
    -> cm
