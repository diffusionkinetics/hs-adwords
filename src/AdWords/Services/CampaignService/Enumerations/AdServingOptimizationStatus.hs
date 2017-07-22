module AdWords.Services.CampaignService.Enumeration.AdServingOptimizationStatus where

data Enumeration = 
    OPTIMIZE 
  | CONVERSION_OPTIMIZE 
  | ROTATE 
  | ROTATE_INDEFINITELY 
  | UNAVAILABLE 
  | UNKNOWN
    deriving Show
