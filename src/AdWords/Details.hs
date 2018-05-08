{-# language TupleSections, RankNTypes #-}

module AdWords.Details where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as BL

import AdWords 
import AdWords.Types
import AdWords.Service

import Network.HTTP.Client (Response)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Data.Monoid 
import Data.Maybe (catMaybes)
import Data.Map.Strict (Map)
import Text.XML
import Lens.Micro
import Lens.Micro.Extras (view)
import Lens.Micro.Internal (foldMapOf)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer.Lazy
import Control.Monad.Trans.Maybe
import Control.Applicative hiding (empty, many)
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

adStats :: MonadIO m =>AdWords m (Either String (Vector AdStats))
adStats = 
  reportAWQL 
    "select Id, Impressions, CreativeQualityScore, Clicks, AveragePosition, Cost from KEYWORDS_PERFORMANCE_REPORT"
    "CSV"
  <&> decode HasHeader . BL.unlines . drop 1 . BL.lines . responseBody

------

pauseAd :: MonadIO m => Int -> Int -> AdWords m (Response [Value])
pauseAd adGroup_id ad_id = do
  request AdGroupAdService $
    "mutate" # 
      "operations" # do
        "operator" ## "SET"
        "operand" # do
          "adGroupId" ## tshow adGroup_id
          "ad" #
            "id" ## tshow ad_id
          "status" ## "PAUSED"
  <&> fmap rval          

enableAd :: MonadIO m => Int -> Int -> AdWords m (Response [Value])
enableAd adGroup_id ad_id = do
  request AdGroupAdService $
    "mutate" #
      "operations" # do
        "operator" ## "SET"
        "operand" # do
          "adGroupId" ## tshow adGroup_id
          "ad" #
            "id" ## tshow ad_id
          "status" ## "ENABLED"
  <&> fmap rval

changeBudget :: MonadIO m => Int -> Int -> AdWords m (Response [Value])
changeBudget campId budgetId = do
  request CampaignService $
    "mutate" # 
      "operations" # do
        "operator" ## "SET"
        "operand" # do
          "id" ## tshow campId
          "budget" # do
            "budgetId" ## tshow budgetId
  <&> fmap rval

changeBidding :: MonadIO m => Int -> Int -> AdWords m (Response [Value])
changeBidding campId bidId = do
  request CampaignService $
    "mutate" #
      "operations" # do
        "operator" ## "SET"
        "operand" # do 
          "id" ## tshow campId
          "biddingStrategyConfiguration" #
            "biddingStrategyId" ## tshow bidId
  <&> fmap rval

addExpandedTextAd :: MonadIO m =>
     Int    -- ad goup id
  -> Text   -- headlinePart1
  -> Text   -- headlinePart2
  -> Text   -- description
  -> Text   -- path1 
  -> Text   -- path2
  -> [Text] -- finalUrls
  -> AdWords m (Response [Value])
addExpandedTextAd adGroupId hd1 hd2 desc ph1 ph2 urls = do
  request AdGroupAdService $
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

type Latitude = Double   -- latitude micro degrees
type Longtitude = Double -- longtitude micro degrees
type GeoPoint = Either (Latitude, Longtitude) Address
type Radius = Double

data DistanceUnits = KILOMETERS | MILES deriving Show
data Address =  -- this mustn't contain empty strings
    CityOnly Text
  | FullAddr 
      Text -- streetAddress
      Text -- streetAddress2
      Text -- cityName
      Text -- provinceCode 
      Text -- provinceName
      Text -- postalCode
      Text -- countryCode

data GeoTarget = 
    Location Text -- location name
  | Proximity GeoPoint DistanceUnits Radius

addCampaignCriterion :: MonadIO m =>
     Int 
  -> GeoTarget 
  -> AdWords m (Response [Value])
addCampaignCriterion camp_id target = 
  mapped . mapped %~ rval $ request CampaignCriterionService $ do
    "mutate" # "operations" # do
      "operator" ## "ADD"
      "operand" # do
        "campaignId" ## tshow camp_id
        case target of
          Location locationName -> 
            "criterion" #? type' "Location" $ 
              "locationName" ## locationName
          Proximity point distanceUnits radius -> 
            let radiusSetting = "radiusDistanceUnits" ## tshow distanceUnits
                                   *> "radiusInUnits" ## tshow radius
            in "criterion" #? type' "Proximity" $
              case point of
                Left (lat, lon) -> 
                  "geoPoint" # do
                    "latitudeInMicroDegrees" ## tshow lat
                    "longtitudeInMicroDegrees" ## tshow lon
                  *> radiusSetting

                Right addr -> do
                  radiusSetting
                  "address" # case addr of
                    CityOnly city ->
                      "cityName" ## city

                    FullAddr str1 str2 city provCode provName postCode countryCode -> do
                      "streetName" ## str1
                      "streetNmae2" ## str2
                      "cityName" ## city
                      "provinceCode" ## provCode
                      "provinceName" ## provName
                      "postalCode" ## postCode
                      "countryCode" ## countryCode

adRemove :: MonadIO m => Int -> Int -> AdWords m (Response Document)
adRemove adId adGroupId = request AdGroupAdService $
  "mutate" # "operations" # do
    "operator" ## "REMOVE"
    "operand" # do
      "adGroupId" ## tshow adGroupId
      "ad" # "id" ## tshow adId
      

adDetails :: MonadIO m => Int -> AdWords m [Value]
adDetails = 
  let askAd :: MonadIO m => 
        Int -> MaybeT (WriterT [Value] (AdWords m)) [Value]
      askAd adId = ask AdGroupAdService . query $
        "select HeadlinePart1, HeadlinePart2, Id, Description, Path1, Path2, AdGroupId, CreativeFinalUrls where Id = " <> tshow adId

      ask :: MonadIO m => 
        Service -> XML -> MaybeT (WriterT [Value] (AdWords m)) [Value]
      ask serv = MaybeT . WriterT . fmap ((\x -> (pure x, x)) . rval . responseBody) . request serv

      find :: MonadIO m => 
        Text -> [Value] -> MaybeT (WriterT [Value] (AdWords m)) Text
      find txt = MaybeT . pure . fmap (view _Content) . Map.lookup txt . Map.unions . foldMap allObjects

      finds :: [Text] -> [Value] -> [Value]
      finds txts = catMaybes . (Map.lookup <$> txts <*>) . pure . Map.unions . foldMap allObjects

      askGroup :: MonadIO m => 
        Text -> MaybeT (WriterT [Value] (AdWords m)) [Value]
      askGroup groupId = ask AdGroupService . query $ 
        "select BiddingStrategyId, CampaignId, Id where Id = " <> groupId

      askCriterion :: MonadIO m => 
        Text -> MaybeT (WriterT [Value] (AdWords m)) [Value]
      askCriterion campId = ask CampaignCriterionService . query $ 
        "select Address, RadiusDistanceUnits, RadiusInUnits, CampaignId where CampaignId = " <> campId

      relevantFields :: [Text]
      relevantFields = 
        [ "headlinePart1"
        , "headlinePart2"
        , "description"
        , "finalUrls"
        , "geoPoint" 
        , "address"
        , "radiusInUnits"
        , "radiusDistanceUnits"
        ]

      aboutAd :: MonadIO m =>
        Int -> MaybeT (WriterT [Value] (AdWords m)) [Value]
      aboutAd = askCriterion <=< find "campaignId" <=< askGroup <=< find "adGroupId" <=< askAd

   in fmap (finds relevantFields) . execWriterT . runMaybeT . aboutAd
     
------

printResponse :: MonadIO m => Service -> XML -> AdWords m ()
printResponse serv body =
  request serv body >>= 
    liftIO . putDoc . vsep . map dshow . rval . responseBody

details :: MonadIO m => Service -> [Text] -> AdWords m (Response [Value])
details serv fields =
  request serv (query $ "select " <> T.intercalate ", " fields)
  <&> fmap rval

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

allObjects :: Value -> [Map Text Value]
allObjects val = concat $ go [val] where
  go :: [Value] -> [[Map Text Value]]
  go vs = objs : foldMap go (objs' <> lists) where
    lists = vs ^.. traverse . _List
    objs  = vs ^.. traverse . _Object
    objs' = vs ^.. traverse . _Object . to (map snd . Map.toList)

_nodes :: Lens' Element [Node]
_nodes f (Element n a ns) = Element n a <$> f ns

_name :: Lens' Element Name
_name  f (Element n a ns) = f n <&> \n' -> Element n' a ns

_NodeElement :: Traversal' Node Element
_NodeElement f (NodeElement el) = NodeElement <$> f el
_NodeElement _ n = pure n

_NodeContent :: Traversal' Node Text
_NodeContent f (NodeContent cnt) = NodeContent <$> f cnt
_NodeContent _ t = pure t

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

named :: Text -> Traversal' Element Element
named byname = filtered ((==) byname . nameLocalName . elementName)

rval :: Document -> [Value]
rval r = r ^.. elements . named "entries" . to ge
{-rval r = r ^.. root . to ge-}
  where
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


docElems :: Document -> [Value]
docElems r = r ^.. elements . to ge
{-rval r = r ^.. root . to ge-}
  where
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

campaignCriterions :: MonadIO m => AdWords m (Response [Value])
campaignCriterions = details CampaignCriterionService selectable
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

biddingStrategies :: MonadIO m => AdWords m (Response [Value])
biddingStrategies = details BiddingStrategyService selectable
  where selectable :: [Text]
        selectable =
          [ "BiddingScheme"
          , "Id"
          , "Name"
          , "Status"
          , "Type"
          ]

adGroupAds :: MonadIO m => AdWords m (Response [Value])
adGroupAds = details AdGroupAdService selectable
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

adGroupFeeds :: MonadIO m => AdWords m (Response [Value])
adGroupFeeds = details AdGroupFeedService selectable
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

adGroups :: MonadIO m => AdWords m (Response [Value])
adGroups = details AdGroupService selectable
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

budgets :: MonadIO m => AdWords m (Response [Value])
budgets = details BudgetService selectable
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

campaigns :: MonadIO m => AdWords m (Response [Value])
campaigns = details CampaignService selectable
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
          --, "TargetSpendEnhancedCpcEnabled"
          , "TargetSpendSpendTarget"
          , "TimeUnit"
          , "TrackingUrlTemplate"
          , "UrlCustomParameters"
          ]

campaignFeeds :: MonadIO m => AdWords m (Response [Value])
campaignFeeds = details CampaignFeedService selectable
  where selectable :: [Text]
        selectable = 
          [ "BaseCampaignId"
          , "CampaignId"
          , "FeedId"
          , "MatchingFunction"
          , "PlaceholderTypes"
          , "Status"
          ]

feeds :: MonadIO m => AdWords m (Response [Value])
feeds = details FeedService selectable
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
