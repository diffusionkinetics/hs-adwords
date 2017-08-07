module AdWords.Details 
  ( adGroups
  , adGroupAds
  , adGroupFeeds
  , budgets
  , campaigns
  , campaignFeeds
  , feeds
  , campaignGroupPerformanceTarget
  ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Monoid 
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Text.XML
import AdWords 
import Lens.Micro
import Lens.Micro.Internal (foldMapOf)
import Lens.Micro.Extras (view)
import Network.HTTP.Client (responseBody)

details service fields = view (to responseBody . pageDetails) <$> request service (query $ "select " <> T.intercalate ", " fields)

named name = filtered ((==) name . nameLocalName . elementName)
elements = root . to allChildren . traverse . _NodeElement

pageDetails = elements . named "rval" . to printEntries

printEntries :: Element -> Doc ann
printEntries (Element n a ns) = line <> pretty (replicate 20 '-') <> line <> vsep (go 0 <$> ns)
  where 
    go :: Int -> Node -> Doc ann
    go 0 (NodeElement (Element (Name n _ _) _ ns)) = pretty n <> colon <> line <+> indent 2 (align (vsep (go 1 <$> ns)))
    go l (NodeElement (Element (Name n _ _) _ ns)) = pretty n <> colon <+> align (vsep (go (l+1) <$> ns))
    go l (NodeContent c) = pretty c

root f (Document p r e) = f r <&> \r -> Document p r e

allChildren :: Element -> [Node]
allChildren el = concat $ go [NodeElement el] where
  go a = a : foldMapOf (traverse . _NodeElement . _nodes) go a

_nodes f (Element n a ns) = Element n a <$> f ns
_name  f (Element n a ns) = f n <&> \n -> Element n a ns

_NodeElement f (NodeElement el) = NodeElement <$> f el
_NodeElement _ n = pure n

_NodeContent f (NodeContent cnt) = NodeContent <$> f cnt
_NodeContent _ n = pure n

_Content f (Content t) = Content <$> f t
_Content _ t = pure t

_Object f (Object m) = Object <$> f m
_Object _ t = pure t

data Value = Content Text | Object (Map Text Value) deriving Show

{-rval :: Document -> Value-}
{-rval r = r ^. elements . named "entries" . to go-}
  {-where-}
    {-go :: Element -> Value-}
    {-go (NodeElement (Element (Name n _ _) _ ns))-}
      {-= Map.singletone n (go <$> ns-}

    {-go (NodeContent c) = Content c-}

{-rval :: Element -> Maybe (Map Text Value)-}
{-rval root = fmap goElem . listToMaybe . findBody $ root-}
  {-where-}
    {-findBody :: Element -> [Element]-}
    {-findBody e@(Element (Name name _ _) _ ns) -}
      {-| T.isInfixOf "Body" name = go ns-}
      {-| otherwise = concat . fmap findBody . go $ ns-}

    {-go :: [Node] -> [Element]-}
    {-go (NodeElement el : xs) = el : go xs-}
    {-go (_ : xs) = go xs-}
    {-go [] = []-}

    {-goElem :: Element -> Map Text Value-}
    {-goElem (Element (Name name _ _) _ ns) -}
      {-| length ns == 1 = Map.singleton name . goNode . head $ ns-}
      {-| length ns >= 2 = Map.singleton name $ List (goNode <$> ns)-}
      {-| otherwise = Map.empty-}

    {-goNode :: Node -> Value-}
    {-goNode n = case n of -}
      {-NodeElement el -> Object $ goElem el-}
      {-NodeContent val -> Content val-}

adGroupAds = details "AdGroupAdService" selectable
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

adGroupFeeds = details "AdGroupFeedService" selectable
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


adGroups = details "AdGroupService" selectable
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

budgets = details "BudgetService" selectable
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

campaigns = details "CampaignService" selectable
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


campaignFeeds = details "CampaignFeedService" selectable
  where selectable :: [Text]
        selectable = 
          [ "BaseCampaignId"
          , "CampaignId"
          , "FeedId"
          , "MatchingFunction"
          , "PlaceholderTypes"
          , "Status"
          ]

feeds = details "FeedService" selectable
  where selectable :: [Text]
        selectable =
          [ "Attributes"
          , "FeedStatus"
          , "Id"
          , "Name"
          , "Origin"
          , "SystemFeedGenerationData"
          ]

campaignGroupPerformanceTarget = view (to responseBody . pageDetails) <$> request "CampaignGroupPerformanceTargetService" (name "get" $ name "selector" $ names "fields" selectable)
  where selectable :: [Text]
        selectable = 
          [ "CampaignGroupId"
          , "EfficiencyTargetType"
          , "EfficiencyTargetValue"
          , "ForecastStatus"
          , "HasPromotedSuggestions"
          , "Id"
          , "SpendTarget"
          , "SpendTargetType"
          , "StartDate"
          , "EndDate"
          , "VolumeGoalType"
          , "VolumeTargetValue"
          ]
