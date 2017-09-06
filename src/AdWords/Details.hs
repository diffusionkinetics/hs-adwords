module AdWords.Details where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as BL

import Network.HTTP.Client (Response)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Data.Monoid 
import Data.Map.Strict (Map)
import Text.XML
import AdWords 
import Lens.Micro
import Lens.Micro.Internal (foldMapOf)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative hiding (many)
import Network.HTTP.Client (responseBody)
import Data.Csv hiding (Name)
import Data.Vector (Vector)
import GHC.Generics (Generic)

data Score = Score Int | NoScore deriving (Generic, Show)
data AdStats = Stats {
    _id :: IdNum
  , _impressions :: Int
  , _qualityScore :: Score
  , _clicks :: Int
  , _avgPosition :: Double
  , _cost :: Double
} deriving (Show, Generic)

data IdNum = 
  IdNum Int | Total deriving (Show, Generic)

instance FromRecord AdStats
instance FromField IdNum where
  parseField s = IdNum <$> parseField s <|> pure Total

instance FromRecord Score
instance FromField Score where
  parseField s = Score <$> parseField s <|> pure NoScore

adStats :: AdWords (Either String (Vector AdStats))
adStats = 
  reportAWQL 
    "select Id, Impressions, CreativeQualityScore, Clicks, AveragePosition, Cost from KEYWORDS_PERFORMANCE_REPORT"
    "CSV"
  <&> decode HasHeader . BL.unlines . drop 1 . BL.lines . responseBody

------

pauseAd :: Int -> Int -> AdWords (Response [Value])
pauseAd adGroup_id ad_id = do
  request "https://adwords.google.com/api/adwords/cm/v201708/AdGroupAdService" $
    "mutate" # 
      "operations" # do
        "operator" ## "SET"
        "operand" # do
          "adGroupId" ## tshow adGroup_id
          "ad" #
            "id" ## tshow ad_id
          "status" ## "PAUSED"
  <&> fmap rval          

enableAd :: Int -> Int -> AdWords (Response [Value])
enableAd adGroup_id ad_id = do
  request "https://adwords.google.com/api/adwords/cm/v201708/AdGroupAdService" $
    "mutate" #
      "operations" # do
        "operator" ## "SET"
        "operand" # do
          "adGroupId" ## tshow adGroup_id
          "ad" #
            "id" ## tshow ad_id
          "status" ## "ENABLED"
  <&> fmap rval

changeBudget :: Int -> Int -> AdWords (Response [Value])
changeBudget campId budgetId = do
  request "https://adwords.google.com/api/adwords/cm/v201708/CampaignService" $
    "mutate" # 
      "operations" # do
        "operator" ## "SET"
        "operand" # do
          "id" ## tshow campId
          "budget" # do
            "budgetId" ## tshow budgetId
  <&> fmap rval

changeBidding :: Int -> Int -> AdWords (Response [Value])
changeBidding campId bidId = do
  request "https://adwords.google.com/api/adwords/cm/v201708/CampaignService" $
    "mutate" #
      "operations" # do
        "operator" ## "SET"
        "operand" # do 
          "id" ## tshow campId
          "biddingStrategyConfiguration" #
            "biddingStrategyId" ## tshow bidId
  <&> fmap rval

addExpandedTextAd :: Int -> Text -> Text -> Text -> Text -> Text -> [Text] -> AdWords (Response [Value])
addExpandedTextAd adGroupId hd1 hd2 desc ph1 ph2 urls = do
  request "https://adwords.google.com/api/adwords/cm/v201708/AdGroupAdService" $
    "mutate" #
      "operations" # do
        "operator" ## "ADD"
        "operand" # do
          "adGroupId" ## tshow adGroupId
          "ad" #? type' "ExpandedTextAd" $ do
            many (name "finalUrls") urls
            "headlinePart1" ## hd1
            "headlinePart2" ## hd2
            "description" ## desc
            "path1" ## ph1
            "path2" ## ph2
  <&> fmap rval

------
printResponse :: String -> XML -> AdWords ()
printResponse url body =
  request url body >>= 
    liftIO . putDoc . vsep . map dshow . rval . responseBody

details :: String -> [Text] -> AdWords (Response [Value])
details serviceUrl fields =
  request serviceUrl (query $ "select " <> T.intercalate ", " fields)
  <&> fmap rval

-- data Value = Content Text | List [Value] | Object (Map Text Value) deriving (Show, Eq)

class DocShow thing where dshow :: thing -> Doc ann
instance DocShow Value where
  dshow val = line <> pretty (replicate 20 '-') <> line <> go val
    where
      go :: Value -> Doc ann
      go (Content c) = pretty c
      go (List ls) = vsep (go <$> ls)
      go (Object obj) = obj & Map.foldrWithKey
        (\k v doc -> pretty k <> colon <+> align (go v <> doc)) 
        mempty

root :: Lens' Document Element
root f (Document p r e) = f r <&> \r' -> Document p r' e

allChildren :: Element -> [Node]
allChildren el = concat $ go [NodeElement el] where
  go a = a : foldMapOf (traverse . _NodeElement . _nodes) go a

_nodes :: Lens' Element [Node]
_nodes f (Element n a ns) = Element n a <$> f ns

_name :: Lens' Element Name
_name  f (Element n a ns) = f n <&> \n' -> Element n' a ns

_NodeElement :: Traversal' Node Element
_NodeElement f (NodeElement el) = NodeElement <$> f el
_NodeElement _ n = pure n

_NodeContent :: Traversal' Node Text
_NodeContent f (NodeContent cnt) = NodeContent <$> f cnt
_NodeContent _ n = pure n

_Content :: Traversal' Value Text
_Content f (Content t) = Content <$> f t
_Content _ t = pure t

_Object :: Traversal' Value (Map Text Value)
_Object f (Object m) = Object <$> f m
_Object _ t = pure t

_List :: Traversal' Value [Value]
_List f (List ls) = List <$> f ls
_List _ t = pure t

data Value = Content Text | List [Value] | Object (Map Text Value) deriving (Show, Eq)

rval :: Document -> [Value]
{-rval r = r ^.. root . to ge-}
rval r = r ^.. elements . named "entries" . to ge
  where
    named byname = filtered ((==) byname . nameLocalName . elementName)
    elements = root . to allChildren . traverse . _NodeElement

    ge :: Element -> Value
    ge (Element (Name n _ _) _ ns) 
      | length ns == 1 = Object . Map.singleton n . gn . head $ ns
      | otherwise =      Object . Map.singleton n . List $ gn <$> ns
        
    gn :: Node -> Value
    gn n = case n of
      NodeElement el -> ge el
      NodeContent c  -> Content c
      NodeComment _  -> Content "not supported content"
      NodeInstruction _ -> Content "not supported content"

campaignCriterions :: AdWords (Response [Value])
campaignCriterions = details "https://adwords.google.com/api/adwords/cm/v201708/CampaignCriterionService" selectable
  where selectable :: [Text]
        selectable =
          [ "Address"
          , "AgeRangeType"
          , "AppId"
          , "BaseCampaignId"
          , "BidModifier"
          , "CampaignCriterionStatus"
          , "CampaignId"
          , "CarrierCountryCode"
          , "CarrierName"
          , "ChannelId"
          , "ChannelName"
          , "ContentLabelType"
          , "CriteriaType"
          , "DayOfWeek"
          , "DeviceType"
          , "Dimensions"
          , "DisplayName"
          , "DisplayType"
          , "EndHour"
          , "EndMinute"
          , "FeedId"
          , "GenderType"
          , "GeoPoint"
          , "Id"
          , "IncomeRangeType"
          , "IpAddress"
          , "IsNegative"
          , "KeywordMatchType"
          , "KeywordText"
          , "LanguageCode"
          , "LanguageName"
          , "LocationName"
          , "ManufacturerName"
          , "MatchingFunction"
          , "MobileAppCategoryId"
          , "OperatingSystemName"
          , "OperatorType"
          , "OsMajorVersion"
          , "OsMinorVersion"
          , "Parameter"
          , "ParentLocations"
          , "ParentType"
          , "Path"
          , "PlacementUrl"
          , "PlatformName"
          , "RadiusDistanceUnits"
          , "StartHour"
          , "StartMinute"
          , "TargetingStatus"
          , "UserInterestId"
          , "UserInterestName"
          , "UserInterestParentId"
          , "UserListEligibleForDisplay"
          , "UserListEligibleForSearch"
          , "UserListId"
          , "UserListMembershipStatus"
          , "UserListName"
          , "VerticalId"
          , "VerticalParentId"
          , "VideoId"
          , "VideoName"
          ]


biddingStrategies :: AdWords (Response [Value])
biddingStrategies = details "https://adwords.google.com/api/adwords/cm/v201708/BiddingStrategyService" selectable
  where selectable :: [Text]
        selectable =
          [ "BiddingScheme"
          , "Id"
          , "Name"
          , "Status"
          , "Type"
          ]

adGroupAds :: AdWords (Response [Value])
adGroupAds = details "https://adwords.google.com/api/adwords/cm/v201708/AdGroupAdService" selectable
  where selectable :: [Text]
        selectable = 
          [ "AdGroupId"
          , "AdType"
          , "AdvertisingId"
          , "BaseAdGroupId"
          , "BaseCampaignId"
          , "BusinessName"
          , "CallOnlyAdBusinessName"
          , "CallOnlyAdCallTracked"
          , "CallOnlyAdConversionTypeId"
          , "CallOnlyAdCountryCode"
          , "CallOnlyAdDescription1"
          , "CallOnlyAdDescription2"
          , "CallOnlyAdDisableCallConversion"
          , "CallOnlyAdPhoneNumber"
          , "CallOnlyAdPhoneNumberVerificationUrl"
          , "CreationTime"
          , "CreativeFinalAppUrls"
          , "CreativeFinalMobileUrls"
          , "CreativeTrackingUrlTemplate"
          , "CreativeUrlCustomParameters"
          , "Description"
          , "Description1"
          , "Description2"
          , "DevicePreference"
          , "Dimensions"
          , "DisplayUrl"
          , "ExpandingDirections"
          , "FileSize"
          , "Headline"
          , "HeadlinePart1"
          , "HeadlinePart2"
          , "Height"
          , "Id"
          , "ImageCreativeName"
          , "IndustryStandardCommercialIdentifier"
          , "IsCookieTargeted"
          , "IsTagged"
          , "IsUserInterestTargeted"
          , "Labels"
          , "LogoImage"
          , "LongHeadline"
          , "MarketingImage"
          , "MediaId"
          , "MimeType"
          , "Path1"
          , "Path2"
          , "PolicySummary"
          , "ReadyToPlayOnTheWeb"
          , "ReferenceId"
          , "RichMediaAdCertifiedVendorFormatId"
          , "RichMediaAdDuration"
          , "RichMediaAdImpressionBeaconUrl"
          , "RichMediaAdName"
          , "RichMediaAdSnippet"
          , "RichMediaAdSourceUrl"
          , "RichMediaAdType"
          , "ShortHeadline"
          , "SourceUrl"
          , "Status"
          , "TemplateAdDuration"
          , "TemplateAdName"
          , "TemplateAdUnionId"
          , "TemplateElementFieldName"
          , "TemplateElementFieldText"
          , "TemplateElementFieldType"
          , "TemplateId"
          , "TemplateOriginAdId"
          , "UniqueName"
          , "Url"
          , "UrlData"
          , "Urls"
          , "VideoTypes"
          , "Width"
          , "YouTubeVideoIdString"
          ]

adGroupFeeds :: AdWords (Response [Value])
adGroupFeeds = details "https://adwords.google.com/api/adwords/cm/v201708/AdGroupFeedService" selectable
  where selectable :: [Text]
        selectable = 
          [ "AdGroupId"
          , "BaseAdGroupId"
          , "BaseCampaignId"
          , "FeedId"
          , "MatchingFunction"
          , "PlaceholderTypes"
          , "Status"
          ]

adGroups :: AdWords (Response [Value])
adGroups = details "https://adwords.google.com/api/adwords/cm/v201708/AdGroupService" selectable
  where selectable :: [Text]
        selectable = 
          [ "AdGroupType"
          , "BaseAdGroupId"
          , "BaseCampaignId"
          , "BidType"
          , "BiddingStrategyName"
          , "BiddingStrategySource"
          , "BiddingStrategyType"
          , "CampaignId"
          , "CampaignName"
          , "ContentBidCriterionTypeGroup"
          , "CpcBid"
          , "CpmBid"
          , "EnhancedCpcEnabled"
          , "EnhancedCpcEnabled"
          , "Id"
          , "Labels"
          , "Name"
          , "Settings"
          , "Status"
          , "TargetCpa"
          , "TargetCpaBid"
          , "TargetCpaBidSource"
          , "TargetSpendEnhancedCpcEnabled"
          , "TrackingUrlTemplate"
          , "UrlCustomParameters"
          ]

budgets :: AdWords (Response [Value])
budgets = details "https://adwords.google.com/api/adwords/cm/v201708/BudgetService" selectable
  where selectable :: [Text]
        selectable = 
          [ "Amount"
          , "BudgetId"
          , "BudgetName"
          , "BudgetReferenceCount"
          , "BudgetStatus"
          , "DeliveryMethod"
          , "IsBudgetExplicitlyShared"
          ]

campaigns :: AdWords (Response [Value])
campaigns = details "https://adwords.google.com/api/adwords/cm/v201708/CampaignService" selectable
  where selectable :: [Text]
        selectable =
          [ "AdServingOptimizationStatus"
          , "AdvertisingChannelSubType"
          , "AdvertisingChannelType"
          , "Amount"
          , "BaseCampaignId"
          , "BiddingStrategyId"
          , "BiddingStrategyName"
          , "BiddingStrategyType"
          , "BudgetId"
          , "BudgetName"
          , "BudgetReferenceCount"
          , "BudgetStatus"
          , "CampaignGroupId"
          , "CampaignTrialType"
          , "DeliveryMethod"
          , "Eligible"
          , "EndDate"
          , "EnhancedCpcEnabled"
          , "FrequencyCapMaxImpressions"
          , "Id"
          , "IsBudgetExplicitlyShared"
          , "Labels"
          , "Level"
          , "Name"
          , "RejectionReasons"
          , "SelectiveOptimization"
          , "ServingStatus"
          , "Settings"
          , "StartDate"
          , "Status"
          , "TargetContentNetwork"
          , "TargetCpa"
          , "TargetCpaMaxCpcBidCeiling"
          , "TargetCpaMaxCpcBidFloor"
          , "TargetGoogleSearch"
          , "TargetPartnerSearchNetwork"
          , "TargetRoas"
          , "TargetRoasBidCeiling"
          , "TargetRoasBidFloor"
          , "TargetSearchNetwork"
          , "TargetSpendBidCeiling"
          , "TargetSpendEnhancedCpcEnabled"
          , "TargetSpendSpendTarget"
          , "TimeUnit"
          , "TrackingUrlTemplate"
          , "UrlCustomParameters"
          ]

campaignFeeds :: AdWords (Response [Value])
campaignFeeds = details "https://adwords.google.com/api/adwords/cm/v201708/CampaignFeedService" selectable
  where selectable :: [Text]
        selectable = 
          [ "BaseCampaignId"
          , "CampaignId"
          , "FeedId"
          , "MatchingFunction"
          , "PlaceholderTypes"
          , "Status"
          ]

feeds :: AdWords (Response [Value])
feeds = details "https://adwords.google.com/api/adwords/cm/v201708/FeedService" selectable
  where selectable :: [Text]
        selectable =
          [ "Attributes"
          , "FeedStatus"
          , "Id"
          , "Name"
          , "Origin"
          , "SystemFeedGenerationData"
          ]

{-campaignGroupPerformanceTarget = do -}
  {-let selectable :: [Text]-}
      {-selectable = -}
        {-[ "CampaignGroupId"-}
        {-, "EfficiencyTargetType"-}
        {-, "EfficiencyTargetValue"-}
        {-, "ForecastStatus"-}
        {-, "HasPromotedSuggestions"-}
        {-, "Id"-}
        {-, "SpendTarget"-}
        {-, "SpendTargetType"-}
        {-, "StartDate"-}
        {-, "EndDate"-}
        {-, "VolumeGoalType"-}
        {-, "VolumeTargetValue"-}
        {-]-}

  {-res <- request "CampaignGroupPerformanceTargetService" (name "get" $ name "selector" $ names "fields" selectable)-}
  {-liftIO . putDoc . view (to responseBody . pageDetails . to printEntities) $ res-}
  {-return (rval <$> res)-}
