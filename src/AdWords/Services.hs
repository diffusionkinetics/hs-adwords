{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module AdWords.Services
  where

import Data.Text (Text)
import Data.Tagged 
import Data.List.NonEmpty (NonEmpty)
import Text.XML.Writer
import Text.XML


import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

ns = "https://adwords.google.com/api/adwords/cm/v201705"
name n = Name n (Just ns) Nothing

tshow :: Show a => a -> Text
tshow = T.pack . show

class Service s where
  data Method s
  data Rval s
  call :: Method s -> Tagged (Rval s) XML


