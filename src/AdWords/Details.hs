{-# LANGUAGE TupleSections, LambdaCase #-}
module AdWords.Details 
  {-( adGroups-}
  {-, adGroupAds-}
  {-, adGroupFeeds-}
  {-, budgets-}
  {-, campaigns-}
  {-, campaignFeeds-}
  {-, feeds-}
  {-, campaignGroupPerformanceTarget-}
  {-) -}
  where

import Debug.Trace
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V

import Debug.Trace

import Data.Text (Text)
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Data.Monoid 
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Text.XML
import AdWords 
import Lens.Micro
import Lens.Micro.Internal (foldMapOf)
import Lens.Micro.Extras (view)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Network.HTTP.Client (responseBody)
import Data.Csv
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Data.Attoparsec.ByteString.Char8

data Score = Score Int | NoScore deriving (Generic, Show)
data AdStats = Stats {
    _id :: Identification
  , _impressions :: Int
  , _qualityScore :: Score
  , _clicks :: Int
  , _avgPosition :: Double
  , _cost :: Double
} deriving (Show, Generic)

data Identification = 
  Identification Int | Total deriving (Show, Generic)

instance FromRecord AdStats
instance FromField Identification where
  parseField s = Identification <$> parseField s <|> pure Total

instance FromRecord Score
instance FromField Score where
  parseField s = Score <$> parseField s <|> pure NoScore

adStats :: AdWords (Either String (Vector AdStats))
adStats = 
  reportAWQL 
    "select Id, Impressions, CreativeQualityScore, Clicks, AveragePosition, Cost from KEYWORDS_PERFORMANCE_REPORT"
    "CSV"
  <&> decode HasHeader . BL.unlines . drop 1 . BL.lines . responseBody

details service fields = do
  res <- request service (query $ "select " <> T.intercalate ", " fields)

  let named name = filtered ((==) name . nameLocalName . elementName)
      elements = root . to allChildren . traverse . _NodeElement

  liftIO $ putDoc (res ^. to responseBody . elements . named "rval" . to printEntries)

  return (rval <$> res)

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

_List f (List ls) = List <$> f ls
_List _ t = pure t

data Value = Content Text | List [Value] | Object (Map Text Value) deriving Show

rval :: Document -> [Value]
rval r = r ^.. elements . named "entries" . to ge
  where
    named name = filtered ((==) name . nameLocalName . elementName)
    elements = root . to allChildren . traverse . _NodeElement

    ge :: Element -> Value
    ge (Element (Name n _ _) _ ns) 
      | length ns == 1 = Object . Map.singleton n . gn . head $ ns
      | otherwise =      Object . Map.singleton n . List $ gn <$> ns
        
    gn :: Node -> Value
    gn = \case 
      NodeElement el -> ge el
      NodeContent c  -> Content c

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
