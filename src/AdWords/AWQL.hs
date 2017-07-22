{-# LANGUAGE OverloadedLists #-}
module AdWords.AWQL 
  where

import Numeric.Natural
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.String (IsString(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import AdWords.Services

data AWQL = Statement Select (Maybe Where) (Maybe During) (Maybe OrderBy) (Maybe Limit)

data ReportAWQL = Report From Where During (Maybe OrderBy) (Maybe Limit)

i :: AWQL
i = Statement (Select ["Name"])
              (Just $ Where ["Name" `Contains` (Value "2")])
              Nothing 
              (Just $ OrderBy "Name" ASC)
              Nothing

data Select = Select (NonEmpty ColumnName)
data From = From SourceName
data Where = Where (NonEmpty Condition)
data During = During DateRange
data OrderBy = OrderBy ColumnName Direction 
data Limit = Limit StartIndex PageSize

data Value = Value Text | ValueList (NonEmpty Text)
instance IsString Value where
  fromString = Value . T.pack

data DateRange = Range Int Int
type ColumnName = Text
type SourceName = Text
type StartIndex = Natural
type PageSize = Natural

data Direction = ASC | DESC deriving Show
data Condition =  
    Equal ColumnName Value
  | NotEqual ColumnName Value
  | Greater ColumnName Value
  | GreaterOrEqual ColumnName Value
  | LessOrEqual ColumnName Value
  | Less ColumnName Value
  | In ColumnName Value
  | NotIn ColumnName Value
  | StartsWith ColumnName Value
  | Contains ColumnName Value
  | IContains ColumnName Value -- case insensative
  | DoesNotContain ColumnName Value
  | IDoesNotContain ColumnName Value -- case insensative
  | ContainsAny ColumnName Value
  | ContainsNon ColumnName Value
  | ContainsAll ColumnName Value
