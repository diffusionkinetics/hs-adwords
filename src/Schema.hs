
module Schema where

import Text.XML.Writer (element, document, elementA, ToXML(..), content, pprint)

import Text.XML as P

import Data.Text 

data ParseError = ParseError

class FromXML a where

  parseIt :: Text -> Eiither ParseError a


data ApiError = ApiError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML ApiError where

  toXML e = element "ApiError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML ApiError where

  parseIt :: Text -> ApiError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_apierror doc)

parse_apierror :: P.Document -> ApiError

parse_apierror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = ApiError fieldPath fieldPathElements trigger errorString apiErrorType


data ApiException = ApiException { message :: String
	, applicationExceptionType :: String } deriving (Show, Generic)
instance ToXML ApiException where

  toXML e = element "ApiException" $ do

    element "message" $ content e.message

    element "applicationExceptionType" $ content e.applicationExceptionType

instance FromXML ApiException where

  parseIt :: Text -> ApiException

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_apiexception doc)

parse_apiexception :: P.Document -> ApiException

parse_apiexception (P.Document _ (P.Element _ _ (NodeContent message):(NodeContent applicationExceptionType):xs) _) = ApiException message applicationExceptionType


data ApplicationException = ApplicationException { message :: String
	, applicationExceptionType :: String } deriving (Show, Generic)
instance ToXML ApplicationException where

  toXML e = element "ApplicationException" $ do

    element "message" $ content e.message

    element "applicationExceptionType" $ content e.applicationExceptionType

instance FromXML ApplicationException where

  parseIt :: Text -> ApplicationException

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_applicationexception doc)

parse_applicationexception :: P.Document -> ApplicationException

parse_applicationexception (P.Document _ (P.Element _ _ (NodeContent message):(NodeContent applicationExceptionType):xs) _) = ApplicationException message applicationExceptionType


data AuthenticationError = AuthenticationError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML AuthenticationError where

  toXML e = element "AuthenticationError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML AuthenticationError where

  parseIt :: Text -> AuthenticationError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_authenticationerror doc)

parse_authenticationerror :: P.Document -> AuthenticationError

parse_authenticationerror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = AuthenticationError fieldPath fieldPathElements trigger errorString apiErrorType


data AuthorizationError = AuthorizationError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML AuthorizationError where

  toXML e = element "AuthorizationError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML AuthorizationError where

  parseIt :: Text -> AuthorizationError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_authorizationerror doc)

parse_authorizationerror :: P.Document -> AuthorizationError

parse_authorizationerror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = AuthorizationError fieldPath fieldPathElements trigger errorString apiErrorType


data ClientTermsError = ClientTermsError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML ClientTermsError where

  toXML e = element "ClientTermsError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML ClientTermsError where

  parseIt :: Text -> ClientTermsError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_clienttermserror doc)

parse_clienttermserror :: P.Document -> ClientTermsError

parse_clienttermserror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = ClientTermsError fieldPath fieldPathElements trigger errorString apiErrorType


data CollectionSizeError = CollectionSizeError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML CollectionSizeError where

  toXML e = element "CollectionSizeError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML CollectionSizeError where

  parseIt :: Text -> CollectionSizeError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_collectionsizeerror doc)

parse_collectionsizeerror :: P.Document -> CollectionSizeError

parse_collectionsizeerror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = CollectionSizeError fieldPath fieldPathElements trigger errorString apiErrorType


data DatabaseError = DatabaseError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML DatabaseError where

  toXML e = element "DatabaseError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML DatabaseError where

  parseIt :: Text -> DatabaseError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_databaseerror doc)

parse_databaseerror :: P.Document -> DatabaseError

parse_databaseerror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = DatabaseError fieldPath fieldPathElements trigger errorString apiErrorType


data Date = Date { year :: Int
	, month :: Int
	, day :: Int } deriving (Show, Generic)
instance ToXML Date where

  toXML e = element "Date" $ do

    element "year" $ content e.year

    element "month" $ content e.month

    element "day" $ content e.day

instance FromXML Date where

  parseIt :: Text -> Date

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_date doc)

parse_date :: P.Document -> Date

parse_date (P.Document _ (P.Element _ _ (NodeContent year):(NodeContent month):(NodeContent day):xs) _) = Date year month day


data DateError = DateError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML DateError where

  toXML e = element "DateError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML DateError where

  parseIt :: Text -> DateError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_dateerror doc)

parse_dateerror :: P.Document -> DateError

parse_dateerror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = DateError fieldPath fieldPathElements trigger errorString apiErrorType


data DateRange = DateRange { min :: Date
	, max :: Date } deriving (Show, Generic)
instance ToXML DateRange where

  toXML e = element "DateRange" $ do

    element "min" $ content e.min

    element "max" $ content e.max

instance FromXML DateRange where

  parseIt :: Text -> DateRange

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_daterange doc)

parse_daterange :: P.Document -> DateRange

parse_daterange (P.Document _ (P.Element _ _ (NodeContent min):(NodeContent max):xs) _) = DateRange min max


data DistinctError = DistinctError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML DistinctError where

  toXML e = element "DistinctError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML DistinctError where

  parseIt :: Text -> DistinctError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_distincterror doc)

parse_distincterror :: P.Document -> DistinctError

parse_distincterror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = DistinctError fieldPath fieldPathElements trigger errorString apiErrorType


data FieldPathElement = FieldPathElement { field :: String
	, index :: Int } deriving (Show, Generic)
instance ToXML FieldPathElement where

  toXML e = element "FieldPathElement" $ do

    element "field" $ content e.field

    element "index" $ content e.index

instance FromXML FieldPathElement where

  parseIt :: Text -> FieldPathElement

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_fieldpathelement doc)

parse_fieldpathelement :: P.Document -> FieldPathElement

parse_fieldpathelement (P.Document _ (P.Element _ _ (NodeContent field):(NodeContent index):xs) _) = FieldPathElement field index


data IdError = IdError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML IdError where

  toXML e = element "IdError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML IdError where

  parseIt :: Text -> IdError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_iderror doc)

parse_iderror :: P.Document -> IdError

parse_iderror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = IdError fieldPath fieldPathElements trigger errorString apiErrorType


data InternalApiError = InternalApiError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML InternalApiError where

  toXML e = element "InternalApiError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML InternalApiError where

  parseIt :: Text -> InternalApiError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_internalapierror doc)

parse_internalapierror :: P.Document -> InternalApiError

parse_internalapierror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = InternalApiError fieldPath fieldPathElements trigger errorString apiErrorType


data NotEmptyError = NotEmptyError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML NotEmptyError where

  toXML e = element "NotEmptyError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML NotEmptyError where

  parseIt :: Text -> NotEmptyError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_notemptyerror doc)

parse_notemptyerror :: P.Document -> NotEmptyError

parse_notemptyerror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = NotEmptyError fieldPath fieldPathElements trigger errorString apiErrorType


data NullError = NullError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML NullError where

  toXML e = element "NullError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML NullError where

  parseIt :: Text -> NullError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_nullerror doc)

parse_nullerror :: P.Document -> NullError

parse_nullerror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = NullError fieldPath fieldPathElements trigger errorString apiErrorType


data Operation = Operation { operator :: Operator
	, operationType :: String } deriving (Show, Generic)
instance ToXML Operation where

  toXML e = element "Operation" $ do

    element "operator" $ content e.operator

    element "operationType" $ content e.operationType

instance FromXML Operation where

  parseIt :: Text -> Operation

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_operation doc)

parse_operation :: P.Document -> Operation

parse_operation (P.Document _ (P.Element _ _ (NodeContent operator):(NodeContent operationType):xs) _) = Operation operator operationType


data OperationAccessDenied = OperationAccessDenied { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML OperationAccessDenied where

  toXML e = element "OperationAccessDenied" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML OperationAccessDenied where

  parseIt :: Text -> OperationAccessDenied

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_operationaccessdenied doc)

parse_operationaccessdenied :: P.Document -> OperationAccessDenied

parse_operationaccessdenied (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = OperationAccessDenied fieldPath fieldPathElements trigger errorString apiErrorType


data OperatorError = OperatorError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML OperatorError where

  toXML e = element "OperatorError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML OperatorError where

  parseIt :: Text -> OperatorError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_operatorerror doc)

parse_operatorerror :: P.Document -> OperatorError

parse_operatorerror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = OperatorError fieldPath fieldPathElements trigger errorString apiErrorType


data OrderBy = OrderBy { field :: String
	, sortOrder :: SortOrder } deriving (Show, Generic)
instance ToXML OrderBy where

  toXML e = element "OrderBy" $ do

    element "field" $ content e.field

    element "sortOrder" $ content e.sortOrder

instance FromXML OrderBy where

  parseIt :: Text -> OrderBy

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_orderby doc)

parse_orderby :: P.Document -> OrderBy

parse_orderby (P.Document _ (P.Element _ _ (NodeContent field):(NodeContent sortOrder):xs) _) = OrderBy field sortOrder


data Paging = Paging { startIndex :: Int
	, numberResults :: Int } deriving (Show, Generic)
instance ToXML Paging where

  toXML e = element "Paging" $ do

    element "startIndex" $ content e.startIndex

    element "numberResults" $ content e.numberResults

instance FromXML Paging where

  parseIt :: Text -> Paging

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_paging doc)

parse_paging :: P.Document -> Paging

parse_paging (P.Document _ (P.Element _ _ (NodeContent startIndex):(NodeContent numberResults):xs) _) = Paging startIndex numberResults


data Predicate = Predicate { field :: String
	, operator :: PredicateOperator
	, values :: String } deriving (Show, Generic)
instance ToXML Predicate where

  toXML e = element "Predicate" $ do

    element "field" $ content e.field

    element "operator" $ content e.operator

    element "values" $ content e.values

instance FromXML Predicate where

  parseIt :: Text -> Predicate

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_predicate doc)

parse_predicate :: P.Document -> Predicate

parse_predicate (P.Document _ (P.Element _ _ (NodeContent field):(NodeContent operator):(NodeContent values):xs) _) = Predicate field operator values


data QuotaCheckError = QuotaCheckError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML QuotaCheckError where

  toXML e = element "QuotaCheckError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML QuotaCheckError where

  parseIt :: Text -> QuotaCheckError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_quotacheckerror doc)

parse_quotacheckerror :: P.Document -> QuotaCheckError

parse_quotacheckerror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = QuotaCheckError fieldPath fieldPathElements trigger errorString apiErrorType


data RangeError = RangeError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML RangeError where

  toXML e = element "RangeError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML RangeError where

  parseIt :: Text -> RangeError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_rangeerror doc)

parse_rangeerror :: P.Document -> RangeError

parse_rangeerror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = RangeError fieldPath fieldPathElements trigger errorString apiErrorType


data RateExceededError = RateExceededError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML RateExceededError where

  toXML e = element "RateExceededError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML RateExceededError where

  parseIt :: Text -> RateExceededError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_rateexceedederror doc)

parse_rateexceedederror :: P.Document -> RateExceededError

parse_rateexceedederror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = RateExceededError fieldPath fieldPathElements trigger errorString apiErrorType


data ReadOnlyError = ReadOnlyError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML ReadOnlyError where

  toXML e = element "ReadOnlyError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML ReadOnlyError where

  parseIt :: Text -> ReadOnlyError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_readonlyerror doc)

parse_readonlyerror :: P.Document -> ReadOnlyError

parse_readonlyerror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = ReadOnlyError fieldPath fieldPathElements trigger errorString apiErrorType


data RegionCodeError = RegionCodeError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML RegionCodeError where

  toXML e = element "RegionCodeError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML RegionCodeError where

  parseIt :: Text -> RegionCodeError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_regioncodeerror doc)

parse_regioncodeerror :: P.Document -> RegionCodeError

parse_regioncodeerror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = RegionCodeError fieldPath fieldPathElements trigger errorString apiErrorType


data RejectedError = RejectedError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML RejectedError where

  toXML e = element "RejectedError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML RejectedError where

  parseIt :: Text -> RejectedError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_rejectederror doc)

parse_rejectederror :: P.Document -> RejectedError

parse_rejectederror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = RejectedError fieldPath fieldPathElements trigger errorString apiErrorType


data RequestError = RequestError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML RequestError where

  toXML e = element "RequestError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML RequestError where

  parseIt :: Text -> RequestError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_requesterror doc)

parse_requesterror :: P.Document -> RequestError

parse_requesterror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = RequestError fieldPath fieldPathElements trigger errorString apiErrorType


data RequiredError = RequiredError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML RequiredError where

  toXML e = element "RequiredError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML RequiredError where

  parseIt :: Text -> RequiredError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_requirederror doc)

parse_requirederror :: P.Document -> RequiredError

parse_requirederror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = RequiredError fieldPath fieldPathElements trigger errorString apiErrorType


data Selector = Selector { fields :: String
	, predicates :: Predicate
	, dateRange :: DateRange
	, ordering :: OrderBy
	, paging :: Paging } deriving (Show, Generic)
instance ToXML Selector where

  toXML e = element "Selector" $ do

    element "fields" $ content e.fields

    element "predicates" $ content e.predicates

    element "dateRange" $ content e.dateRange

    element "ordering" $ content e.ordering

    element "paging" $ content e.paging

instance FromXML Selector where

  parseIt :: Text -> Selector

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_selector doc)

parse_selector :: P.Document -> Selector

parse_selector (P.Document _ (P.Element _ _ (NodeContent fields):(NodeContent predicates):(NodeContent dateRange):(NodeContent ordering):(NodeContent paging):xs) _) = Selector fields predicates dateRange ordering paging


data SelectorError = SelectorError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML SelectorError where

  toXML e = element "SelectorError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML SelectorError where

  parseIt :: Text -> SelectorError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_selectorerror doc)

parse_selectorerror :: P.Document -> SelectorError

parse_selectorerror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = SelectorError fieldPath fieldPathElements trigger errorString apiErrorType


data SizeLimitError = SizeLimitError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML SizeLimitError where

  toXML e = element "SizeLimitError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML SizeLimitError where

  parseIt :: Text -> SizeLimitError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_sizelimiterror doc)

parse_sizelimiterror :: P.Document -> SizeLimitError

parse_sizelimiterror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = SizeLimitError fieldPath fieldPathElements trigger errorString apiErrorType


data SoapHeader = SoapHeader { clientCustomerId :: String
	, developerToken :: String
	, userAgent :: String
	, validateOnly :: boolean
	, partialFailure :: boolean } deriving (Show, Generic)
instance ToXML SoapHeader where

  toXML e = element "SoapHeader" $ do

    element "clientCustomerId" $ content e.clientCustomerId

    element "developerToken" $ content e.developerToken

    element "userAgent" $ content e.userAgent

    element "validateOnly" $ content e.validateOnly

    element "partialFailure" $ content e.partialFailure

instance FromXML SoapHeader where

  parseIt :: Text -> SoapHeader

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_soapheader doc)

parse_soapheader :: P.Document -> SoapHeader

parse_soapheader (P.Document _ (P.Element _ _ (NodeContent clientCustomerId):(NodeContent developerToken):(NodeContent userAgent):(NodeContent validateOnly):(NodeContent partialFailure):xs) _) = SoapHeader clientCustomerId developerToken userAgent validateOnly partialFailure


data SoapResponseHeader = SoapResponseHeader { requestId :: String
	, serviceName :: String
	, methodName :: String
	, operations :: Int
	, responseTime :: Int } deriving (Show, Generic)
instance ToXML SoapResponseHeader where

  toXML e = element "SoapResponseHeader" $ do

    element "requestId" $ content e.requestId

    element "serviceName" $ content e.serviceName

    element "methodName" $ content e.methodName

    element "operations" $ content e.operations

    element "responseTime" $ content e.responseTime

instance FromXML SoapResponseHeader where

  parseIt :: Text -> SoapResponseHeader

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_soapresponseheader doc)

parse_soapresponseheader :: P.Document -> SoapResponseHeader

parse_soapresponseheader (P.Document _ (P.Element _ _ (NodeContent requestId):(NodeContent serviceName):(NodeContent methodName):(NodeContent operations):(NodeContent responseTime):xs) _) = SoapResponseHeader requestId serviceName methodName operations responseTime


data StringFormatError = StringFormatError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML StringFormatError where

  toXML e = element "StringFormatError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML StringFormatError where

  parseIt :: Text -> StringFormatError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_stringformaterror doc)

parse_stringformaterror :: P.Document -> StringFormatError

parse_stringformaterror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = StringFormatError fieldPath fieldPathElements trigger errorString apiErrorType


data StringLengthError = StringLengthError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML StringLengthError where

  toXML e = element "StringLengthError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML StringLengthError where

  parseIt :: Text -> StringLengthError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_stringlengtherror doc)

parse_stringlengtherror :: P.Document -> StringLengthError

parse_stringlengtherror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = StringLengthError fieldPath fieldPathElements trigger errorString apiErrorType

type AuthenticationError.Reason = String

type AuthorizationError.Reason = String

type ClientTermsError.Reason = String

type CollectionSizeError.Reason = String

type DatabaseError.Reason = String

type DateError.Reason = String

type DistinctError.Reason = String

type IdError.Reason = String

type InternalApiError.Reason = String

type NotEmptyError.Reason = String

type NullError.Reason = String

type OperationAccessDenied.Reason = String

type Operator = String

type OperatorError.Reason = String

type Predicate.Operator = String

type QuotaCheckError.Reason = String

type RangeError.Reason = String

type RateExceededError.Reason = String

type ReadOnlyError.Reason = String

type RegionCodeError.Reason = String

type RejectedError.Reason = String

type RequestError.Reason = String

type RequiredError.Reason = String

type SelectorError.Reason = String

type SizeLimitError.Reason = String

type SortOrder = String

type StringFormatError.Reason = String

type StringLengthError.Reason = String


data AccountLabelPage = AccountLabelPage { labels :: AccountLabel } deriving (Show, Generic)
instance ToXML AccountLabelPage where

  toXML e = element "AccountLabelPage" $ do

    element "labels" $ content e.labels

instance FromXML AccountLabelPage where

  parseIt :: Text -> AccountLabelPage

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_accountlabelpage doc)

parse_accountlabelpage :: P.Document -> AccountLabelPage

parse_accountlabelpage (P.Document _ (P.Element _ _ (NodeContent labels):xs) _) = AccountLabelPage labels


data AccountLabelReturnValue = AccountLabelReturnValue { labels :: AccountLabel } deriving (Show, Generic)
instance ToXML AccountLabelReturnValue where

  toXML e = element "AccountLabelReturnValue" $ do

    element "labels" $ content e.labels

instance FromXML AccountLabelReturnValue where

  parseIt :: Text -> AccountLabelReturnValue

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_accountlabelreturnvalue doc)

parse_accountlabelreturnvalue :: P.Document -> AccountLabelReturnValue

parse_accountlabelreturnvalue (P.Document _ (P.Element _ _ (NodeContent labels):xs) _) = AccountLabelReturnValue labels


data CurrencyCodeError = CurrencyCodeError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML CurrencyCodeError where

  toXML e = element "CurrencyCodeError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML CurrencyCodeError where

  parseIt :: Text -> CurrencyCodeError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_currencycodeerror doc)

parse_currencycodeerror :: P.Document -> CurrencyCodeError

parse_currencycodeerror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = CurrencyCodeError fieldPath fieldPathElements trigger errorString apiErrorType


data AccountLabel = AccountLabel { id :: Int
	, name :: String } deriving (Show, Generic)
instance ToXML AccountLabel where

  toXML e = element "AccountLabel" $ do

    element "id" $ content e.id

    element "name" $ content e.name

instance FromXML AccountLabel where

  parseIt :: Text -> AccountLabel

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_accountlabel doc)

parse_accountlabel :: P.Document -> AccountLabel

parse_accountlabel (P.Document _ (P.Element _ _ (NodeContent id):(NodeContent name):xs) _) = AccountLabel id name


data LabelServiceError = LabelServiceError { fieldPath :: String
	, fieldPathElements :: FieldPathElement
	, trigger :: String
	, errorString :: String
	, apiErrorType :: String } deriving (Show, Generic)
instance ToXML LabelServiceError where

  toXML e = element "LabelServiceError" $ do

    element "fieldPath" $ content e.fieldPath

    element "fieldPathElements" $ content e.fieldPathElements

    element "trigger" $ content e.trigger

    element "errorString" $ content e.errorString

    element "apiErrorType" $ content e.apiErrorType

instance FromXML LabelServiceError where

  parseIt :: Text -> LabelServiceError

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_labelserviceerror doc)

parse_labelserviceerror :: P.Document -> LabelServiceError

parse_labelserviceerror (P.Document _ (P.Element _ _ (NodeContent fieldPath):(NodeContent fieldPathElements):(NodeContent trigger):(NodeContent errorString):(NodeContent apiErrorType):xs) _) = LabelServiceError fieldPath fieldPathElements trigger errorString apiErrorType


data AccountLabelOperation = AccountLabelOperation { operator :: Operator
	, operationType :: String } deriving (Show, Generic)
instance ToXML AccountLabelOperation where

  toXML e = element "AccountLabelOperation" $ do

    element "operator" $ content e.operator

    element "operationType" $ content e.operationType

instance FromXML AccountLabelOperation where

  parseIt :: Text -> AccountLabelOperation

  parseIt text = case P.parseText def text of 

    Left err -> Left ParseError

    Right doc -> Right (parse_accountlabeloperation doc)

parse_accountlabeloperation :: P.Document -> AccountLabelOperation

parse_accountlabeloperation (P.Document _ (P.Element _ _ (NodeContent operator):(NodeContent operationType):xs) _) = AccountLabelOperation operator operationType

type CurrencyCodeError.Reason = String

type LabelServiceError.Reason = String

