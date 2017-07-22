module AdWords.Services.CampaignService.Enumeration.RejectionReason where

data Enumeration = 
    CAMPAIGN_IS_NOT_ACTIVE 
  | NOT_CPC_CAMAPIGN 
  | CONVERSION_TRACKING_NOTENABLED
  | NOT_ENOUGH_CONVERSIONS
  | UNKNOWN
    deriving Show
