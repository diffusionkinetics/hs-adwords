{-# LANGUAGE OverloadedStrings #-}
module Schema where
import Control.Monad (forM_)
import Text.XML.Writer (element, document, elementA, ToXML(..), content, pprint)
import Text.XML as P
import Data.Text 
import ParseXML
import Data.Default (Default(..))
data ParseError = ParseError


data ApiError = ApiError { apiErrorfieldPath :: Maybe Text
  , apiErrorfieldPathElements :: [FieldPathElement]
  , apiErrortrigger :: Maybe Text
  , apiErrorerrorString :: Maybe Text
  , apiErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default ApiError where
  def = ApiError { apiErrorfieldPath = def, apiErrorfieldPathElements = def, apiErrortrigger = def, apiErrorerrorString = def, apiErrorApiErrorType = def }
instance ToXML ApiError where
  toXML e = element "ApiError" $ do
    case (apiErrorfieldPath e) of
        (Just x) -> element "apiErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (apiErrorfieldPathElements e) $ \x -> element "apiErrorfieldPathElements" $ toXML x
    case (apiErrortrigger e) of
        (Just x) -> element "apiErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (apiErrorerrorString e) of
        (Just x) -> element "apiErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (apiErrorApiErrorType e) of
        (Just x) -> element "apiErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML ApiError where
  parse = do
    _apiErrorfieldPath <- maybeElementContent "apiErrorfieldPath"
    _apiErrorfieldPathElements <- multipleElementContent "apiErrorfieldPathElements"
    _apiErrortrigger <- maybeElementContent "apiErrortrigger"
    _apiErrorerrorString <- maybeElementContent "apiErrorerrorString"
    _apiErrorApiErrorType <- maybeElementContent "apiErrorApiErrorType"
    return $ ApiError {
      apiErrorfieldPath = _apiErrorfieldPath
      , apiErrorfieldPathElements = _apiErrorfieldPathElements
      , apiErrortrigger = _apiErrortrigger
      , apiErrorerrorString = _apiErrorerrorString
      , apiErrorApiErrorType = _apiErrorApiErrorType
      }


data ApiException = ApiException { apiExceptionmessage :: Maybe Text
  , apiExceptionApplicationExceptionType :: Maybe Text } deriving (Show)
instance Default ApiException where
  def = ApiException { apiExceptionmessage = def, apiExceptionApplicationExceptionType = def }
instance ToXML ApiException where
  toXML e = element "ApiException" $ do
    case (apiExceptionmessage e) of
        (Just x) -> element "apiExceptionmessage" $ toXML x
        (Nothing) -> content ""
    case (apiExceptionApplicationExceptionType e) of
        (Just x) -> element "apiExceptionApplicationExceptionType" $ toXML x
        (Nothing) -> content ""
instance FromXML ApiException where
  parse = do
    _apiExceptionmessage <- maybeElementContent "apiExceptionmessage"
    _apiExceptionApplicationExceptionType <- maybeElementContent "apiExceptionApplicationExceptionType"
    return $ ApiException {
      apiExceptionmessage = _apiExceptionmessage
      , apiExceptionApplicationExceptionType = _apiExceptionApplicationExceptionType
      }


data ApplicationException = ApplicationException { applicationExceptionmessage :: Maybe Text
  , applicationExceptionApplicationExceptionType :: Maybe Text } deriving (Show)
instance Default ApplicationException where
  def = ApplicationException { applicationExceptionmessage = def, applicationExceptionApplicationExceptionType = def }
instance ToXML ApplicationException where
  toXML e = element "ApplicationException" $ do
    case (applicationExceptionmessage e) of
        (Just x) -> element "applicationExceptionmessage" $ toXML x
        (Nothing) -> content ""
    case (applicationExceptionApplicationExceptionType e) of
        (Just x) -> element "applicationExceptionApplicationExceptionType" $ toXML x
        (Nothing) -> content ""
instance FromXML ApplicationException where
  parse = do
    _applicationExceptionmessage <- maybeElementContent "applicationExceptionmessage"
    _applicationExceptionApplicationExceptionType <- maybeElementContent "applicationExceptionApplicationExceptionType"
    return $ ApplicationException {
      applicationExceptionmessage = _applicationExceptionmessage
      , applicationExceptionApplicationExceptionType = _applicationExceptionApplicationExceptionType
      }


data AuthenticationError = AuthenticationError { authenticationErrorfieldPath :: Maybe Text
  , authenticationErrorfieldPathElements :: [FieldPathElement]
  , authenticationErrortrigger :: Maybe Text
  , authenticationErrorerrorString :: Maybe Text
  , authenticationErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default AuthenticationError where
  def = AuthenticationError { authenticationErrorfieldPath = def, authenticationErrorfieldPathElements = def, authenticationErrortrigger = def, authenticationErrorerrorString = def, authenticationErrorApiErrorType = def }
instance ToXML AuthenticationError where
  toXML e = element "AuthenticationError" $ do
    case (authenticationErrorfieldPath e) of
        (Just x) -> element "authenticationErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (authenticationErrorfieldPathElements e) $ \x -> element "authenticationErrorfieldPathElements" $ toXML x
    case (authenticationErrortrigger e) of
        (Just x) -> element "authenticationErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (authenticationErrorerrorString e) of
        (Just x) -> element "authenticationErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (authenticationErrorApiErrorType e) of
        (Just x) -> element "authenticationErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML AuthenticationError where
  parse = do
    _authenticationErrorfieldPath <- maybeElementContent "authenticationErrorfieldPath"
    _authenticationErrorfieldPathElements <- multipleElementContent "authenticationErrorfieldPathElements"
    _authenticationErrortrigger <- maybeElementContent "authenticationErrortrigger"
    _authenticationErrorerrorString <- maybeElementContent "authenticationErrorerrorString"
    _authenticationErrorApiErrorType <- maybeElementContent "authenticationErrorApiErrorType"
    return $ AuthenticationError {
      authenticationErrorfieldPath = _authenticationErrorfieldPath
      , authenticationErrorfieldPathElements = _authenticationErrorfieldPathElements
      , authenticationErrortrigger = _authenticationErrortrigger
      , authenticationErrorerrorString = _authenticationErrorerrorString
      , authenticationErrorApiErrorType = _authenticationErrorApiErrorType
      }


data AuthorizationError = AuthorizationError { authorizationErrorfieldPath :: Maybe Text
  , authorizationErrorfieldPathElements :: [FieldPathElement]
  , authorizationErrortrigger :: Maybe Text
  , authorizationErrorerrorString :: Maybe Text
  , authorizationErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default AuthorizationError where
  def = AuthorizationError { authorizationErrorfieldPath = def, authorizationErrorfieldPathElements = def, authorizationErrortrigger = def, authorizationErrorerrorString = def, authorizationErrorApiErrorType = def }
instance ToXML AuthorizationError where
  toXML e = element "AuthorizationError" $ do
    case (authorizationErrorfieldPath e) of
        (Just x) -> element "authorizationErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (authorizationErrorfieldPathElements e) $ \x -> element "authorizationErrorfieldPathElements" $ toXML x
    case (authorizationErrortrigger e) of
        (Just x) -> element "authorizationErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (authorizationErrorerrorString e) of
        (Just x) -> element "authorizationErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (authorizationErrorApiErrorType e) of
        (Just x) -> element "authorizationErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML AuthorizationError where
  parse = do
    _authorizationErrorfieldPath <- maybeElementContent "authorizationErrorfieldPath"
    _authorizationErrorfieldPathElements <- multipleElementContent "authorizationErrorfieldPathElements"
    _authorizationErrortrigger <- maybeElementContent "authorizationErrortrigger"
    _authorizationErrorerrorString <- maybeElementContent "authorizationErrorerrorString"
    _authorizationErrorApiErrorType <- maybeElementContent "authorizationErrorApiErrorType"
    return $ AuthorizationError {
      authorizationErrorfieldPath = _authorizationErrorfieldPath
      , authorizationErrorfieldPathElements = _authorizationErrorfieldPathElements
      , authorizationErrortrigger = _authorizationErrortrigger
      , authorizationErrorerrorString = _authorizationErrorerrorString
      , authorizationErrorApiErrorType = _authorizationErrorApiErrorType
      }


data ClientTermsError = ClientTermsError { clientTermsErrorfieldPath :: Maybe Text
  , clientTermsErrorfieldPathElements :: [FieldPathElement]
  , clientTermsErrortrigger :: Maybe Text
  , clientTermsErrorerrorString :: Maybe Text
  , clientTermsErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default ClientTermsError where
  def = ClientTermsError { clientTermsErrorfieldPath = def, clientTermsErrorfieldPathElements = def, clientTermsErrortrigger = def, clientTermsErrorerrorString = def, clientTermsErrorApiErrorType = def }
instance ToXML ClientTermsError where
  toXML e = element "ClientTermsError" $ do
    case (clientTermsErrorfieldPath e) of
        (Just x) -> element "clientTermsErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (clientTermsErrorfieldPathElements e) $ \x -> element "clientTermsErrorfieldPathElements" $ toXML x
    case (clientTermsErrortrigger e) of
        (Just x) -> element "clientTermsErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (clientTermsErrorerrorString e) of
        (Just x) -> element "clientTermsErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (clientTermsErrorApiErrorType e) of
        (Just x) -> element "clientTermsErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML ClientTermsError where
  parse = do
    _clientTermsErrorfieldPath <- maybeElementContent "clientTermsErrorfieldPath"
    _clientTermsErrorfieldPathElements <- multipleElementContent "clientTermsErrorfieldPathElements"
    _clientTermsErrortrigger <- maybeElementContent "clientTermsErrortrigger"
    _clientTermsErrorerrorString <- maybeElementContent "clientTermsErrorerrorString"
    _clientTermsErrorApiErrorType <- maybeElementContent "clientTermsErrorApiErrorType"
    return $ ClientTermsError {
      clientTermsErrorfieldPath = _clientTermsErrorfieldPath
      , clientTermsErrorfieldPathElements = _clientTermsErrorfieldPathElements
      , clientTermsErrortrigger = _clientTermsErrortrigger
      , clientTermsErrorerrorString = _clientTermsErrorerrorString
      , clientTermsErrorApiErrorType = _clientTermsErrorApiErrorType
      }


data CollectionSizeError = CollectionSizeError { collectionSizeErrorfieldPath :: Maybe Text
  , collectionSizeErrorfieldPathElements :: [FieldPathElement]
  , collectionSizeErrortrigger :: Maybe Text
  , collectionSizeErrorerrorString :: Maybe Text
  , collectionSizeErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default CollectionSizeError where
  def = CollectionSizeError { collectionSizeErrorfieldPath = def, collectionSizeErrorfieldPathElements = def, collectionSizeErrortrigger = def, collectionSizeErrorerrorString = def, collectionSizeErrorApiErrorType = def }
instance ToXML CollectionSizeError where
  toXML e = element "CollectionSizeError" $ do
    case (collectionSizeErrorfieldPath e) of
        (Just x) -> element "collectionSizeErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (collectionSizeErrorfieldPathElements e) $ \x -> element "collectionSizeErrorfieldPathElements" $ toXML x
    case (collectionSizeErrortrigger e) of
        (Just x) -> element "collectionSizeErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (collectionSizeErrorerrorString e) of
        (Just x) -> element "collectionSizeErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (collectionSizeErrorApiErrorType e) of
        (Just x) -> element "collectionSizeErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML CollectionSizeError where
  parse = do
    _collectionSizeErrorfieldPath <- maybeElementContent "collectionSizeErrorfieldPath"
    _collectionSizeErrorfieldPathElements <- multipleElementContent "collectionSizeErrorfieldPathElements"
    _collectionSizeErrortrigger <- maybeElementContent "collectionSizeErrortrigger"
    _collectionSizeErrorerrorString <- maybeElementContent "collectionSizeErrorerrorString"
    _collectionSizeErrorApiErrorType <- maybeElementContent "collectionSizeErrorApiErrorType"
    return $ CollectionSizeError {
      collectionSizeErrorfieldPath = _collectionSizeErrorfieldPath
      , collectionSizeErrorfieldPathElements = _collectionSizeErrorfieldPathElements
      , collectionSizeErrortrigger = _collectionSizeErrortrigger
      , collectionSizeErrorerrorString = _collectionSizeErrorerrorString
      , collectionSizeErrorApiErrorType = _collectionSizeErrorApiErrorType
      }


data DatabaseError = DatabaseError { databaseErrorfieldPath :: Maybe Text
  , databaseErrorfieldPathElements :: [FieldPathElement]
  , databaseErrortrigger :: Maybe Text
  , databaseErrorerrorString :: Maybe Text
  , databaseErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default DatabaseError where
  def = DatabaseError { databaseErrorfieldPath = def, databaseErrorfieldPathElements = def, databaseErrortrigger = def, databaseErrorerrorString = def, databaseErrorApiErrorType = def }
instance ToXML DatabaseError where
  toXML e = element "DatabaseError" $ do
    case (databaseErrorfieldPath e) of
        (Just x) -> element "databaseErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (databaseErrorfieldPathElements e) $ \x -> element "databaseErrorfieldPathElements" $ toXML x
    case (databaseErrortrigger e) of
        (Just x) -> element "databaseErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (databaseErrorerrorString e) of
        (Just x) -> element "databaseErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (databaseErrorApiErrorType e) of
        (Just x) -> element "databaseErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML DatabaseError where
  parse = do
    _databaseErrorfieldPath <- maybeElementContent "databaseErrorfieldPath"
    _databaseErrorfieldPathElements <- multipleElementContent "databaseErrorfieldPathElements"
    _databaseErrortrigger <- maybeElementContent "databaseErrortrigger"
    _databaseErrorerrorString <- maybeElementContent "databaseErrorerrorString"
    _databaseErrorApiErrorType <- maybeElementContent "databaseErrorApiErrorType"
    return $ DatabaseError {
      databaseErrorfieldPath = _databaseErrorfieldPath
      , databaseErrorfieldPathElements = _databaseErrorfieldPathElements
      , databaseErrortrigger = _databaseErrortrigger
      , databaseErrorerrorString = _databaseErrorerrorString
      , databaseErrorApiErrorType = _databaseErrorApiErrorType
      }


data Date = Date { dateyear :: Maybe Int
  , datemonth :: Maybe Int
  , dateday :: Maybe Int } deriving (Show)
instance Default Date where
  def = Date { dateyear = def, datemonth = def, dateday = def }
instance ToXML Date where
  toXML e = element "Date" $ do
    case (dateyear e) of
        (Just x) -> element "dateyear" $ toXML x
        (Nothing) -> content ""
    case (datemonth e) of
        (Just x) -> element "datemonth" $ toXML x
        (Nothing) -> content ""
    case (dateday e) of
        (Just x) -> element "dateday" $ toXML x
        (Nothing) -> content ""
instance FromXML Date where
  parse = do
    _dateyear <- maybeElementContent "dateyear"
    _datemonth <- maybeElementContent "datemonth"
    _dateday <- maybeElementContent "dateday"
    return $ Date {
      dateyear = _dateyear
      , datemonth = _datemonth
      , dateday = _dateday
      }


data DateError = DateError { dateErrorfieldPath :: Maybe Text
  , dateErrorfieldPathElements :: [FieldPathElement]
  , dateErrortrigger :: Maybe Text
  , dateErrorerrorString :: Maybe Text
  , dateErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default DateError where
  def = DateError { dateErrorfieldPath = def, dateErrorfieldPathElements = def, dateErrortrigger = def, dateErrorerrorString = def, dateErrorApiErrorType = def }
instance ToXML DateError where
  toXML e = element "DateError" $ do
    case (dateErrorfieldPath e) of
        (Just x) -> element "dateErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (dateErrorfieldPathElements e) $ \x -> element "dateErrorfieldPathElements" $ toXML x
    case (dateErrortrigger e) of
        (Just x) -> element "dateErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (dateErrorerrorString e) of
        (Just x) -> element "dateErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (dateErrorApiErrorType e) of
        (Just x) -> element "dateErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML DateError where
  parse = do
    _dateErrorfieldPath <- maybeElementContent "dateErrorfieldPath"
    _dateErrorfieldPathElements <- multipleElementContent "dateErrorfieldPathElements"
    _dateErrortrigger <- maybeElementContent "dateErrortrigger"
    _dateErrorerrorString <- maybeElementContent "dateErrorerrorString"
    _dateErrorApiErrorType <- maybeElementContent "dateErrorApiErrorType"
    return $ DateError {
      dateErrorfieldPath = _dateErrorfieldPath
      , dateErrorfieldPathElements = _dateErrorfieldPathElements
      , dateErrortrigger = _dateErrortrigger
      , dateErrorerrorString = _dateErrorerrorString
      , dateErrorApiErrorType = _dateErrorApiErrorType
      }


data DateRange = DateRange { dateRangemin :: Maybe Date
  , dateRangemax :: Maybe Date } deriving (Show)
instance Default DateRange where
  def = DateRange { dateRangemin = def, dateRangemax = def }
instance ToXML DateRange where
  toXML e = element "DateRange" $ do
    case (dateRangemin e) of
        (Just x) -> element "dateRangemin" $ toXML x
        (Nothing) -> content ""
    case (dateRangemax e) of
        (Just x) -> element "dateRangemax" $ toXML x
        (Nothing) -> content ""
instance FromXML DateRange where
  parse = do
    _dateRangemin <- maybeElementContent "dateRangemin"
    _dateRangemax <- maybeElementContent "dateRangemax"
    return $ DateRange {
      dateRangemin = _dateRangemin
      , dateRangemax = _dateRangemax
      }


data DistinctError = DistinctError { distinctErrorfieldPath :: Maybe Text
  , distinctErrorfieldPathElements :: [FieldPathElement]
  , distinctErrortrigger :: Maybe Text
  , distinctErrorerrorString :: Maybe Text
  , distinctErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default DistinctError where
  def = DistinctError { distinctErrorfieldPath = def, distinctErrorfieldPathElements = def, distinctErrortrigger = def, distinctErrorerrorString = def, distinctErrorApiErrorType = def }
instance ToXML DistinctError where
  toXML e = element "DistinctError" $ do
    case (distinctErrorfieldPath e) of
        (Just x) -> element "distinctErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (distinctErrorfieldPathElements e) $ \x -> element "distinctErrorfieldPathElements" $ toXML x
    case (distinctErrortrigger e) of
        (Just x) -> element "distinctErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (distinctErrorerrorString e) of
        (Just x) -> element "distinctErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (distinctErrorApiErrorType e) of
        (Just x) -> element "distinctErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML DistinctError where
  parse = do
    _distinctErrorfieldPath <- maybeElementContent "distinctErrorfieldPath"
    _distinctErrorfieldPathElements <- multipleElementContent "distinctErrorfieldPathElements"
    _distinctErrortrigger <- maybeElementContent "distinctErrortrigger"
    _distinctErrorerrorString <- maybeElementContent "distinctErrorerrorString"
    _distinctErrorApiErrorType <- maybeElementContent "distinctErrorApiErrorType"
    return $ DistinctError {
      distinctErrorfieldPath = _distinctErrorfieldPath
      , distinctErrorfieldPathElements = _distinctErrorfieldPathElements
      , distinctErrortrigger = _distinctErrortrigger
      , distinctErrorerrorString = _distinctErrorerrorString
      , distinctErrorApiErrorType = _distinctErrorApiErrorType
      }


data FieldPathElement = FieldPathElement { fieldPathElementfield :: Maybe Text
  , fieldPathElementindex :: Maybe Int } deriving (Show)
instance Default FieldPathElement where
  def = FieldPathElement { fieldPathElementfield = def, fieldPathElementindex = def }
instance ToXML FieldPathElement where
  toXML e = element "FieldPathElement" $ do
    case (fieldPathElementfield e) of
        (Just x) -> element "fieldPathElementfield" $ toXML x
        (Nothing) -> content ""
    case (fieldPathElementindex e) of
        (Just x) -> element "fieldPathElementindex" $ toXML x
        (Nothing) -> content ""
instance FromXML FieldPathElement where
  parse = do
    _fieldPathElementfield <- maybeElementContent "fieldPathElementfield"
    _fieldPathElementindex <- maybeElementContent "fieldPathElementindex"
    return $ FieldPathElement {
      fieldPathElementfield = _fieldPathElementfield
      , fieldPathElementindex = _fieldPathElementindex
      }


data IdError = IdError { idErrorfieldPath :: Maybe Text
  , idErrorfieldPathElements :: [FieldPathElement]
  , idErrortrigger :: Maybe Text
  , idErrorerrorString :: Maybe Text
  , idErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default IdError where
  def = IdError { idErrorfieldPath = def, idErrorfieldPathElements = def, idErrortrigger = def, idErrorerrorString = def, idErrorApiErrorType = def }
instance ToXML IdError where
  toXML e = element "IdError" $ do
    case (idErrorfieldPath e) of
        (Just x) -> element "idErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (idErrorfieldPathElements e) $ \x -> element "idErrorfieldPathElements" $ toXML x
    case (idErrortrigger e) of
        (Just x) -> element "idErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (idErrorerrorString e) of
        (Just x) -> element "idErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (idErrorApiErrorType e) of
        (Just x) -> element "idErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML IdError where
  parse = do
    _idErrorfieldPath <- maybeElementContent "idErrorfieldPath"
    _idErrorfieldPathElements <- multipleElementContent "idErrorfieldPathElements"
    _idErrortrigger <- maybeElementContent "idErrortrigger"
    _idErrorerrorString <- maybeElementContent "idErrorerrorString"
    _idErrorApiErrorType <- maybeElementContent "idErrorApiErrorType"
    return $ IdError {
      idErrorfieldPath = _idErrorfieldPath
      , idErrorfieldPathElements = _idErrorfieldPathElements
      , idErrortrigger = _idErrortrigger
      , idErrorerrorString = _idErrorerrorString
      , idErrorApiErrorType = _idErrorApiErrorType
      }


data InternalApiError = InternalApiError { internalApiErrorfieldPath :: Maybe Text
  , internalApiErrorfieldPathElements :: [FieldPathElement]
  , internalApiErrortrigger :: Maybe Text
  , internalApiErrorerrorString :: Maybe Text
  , internalApiErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default InternalApiError where
  def = InternalApiError { internalApiErrorfieldPath = def, internalApiErrorfieldPathElements = def, internalApiErrortrigger = def, internalApiErrorerrorString = def, internalApiErrorApiErrorType = def }
instance ToXML InternalApiError where
  toXML e = element "InternalApiError" $ do
    case (internalApiErrorfieldPath e) of
        (Just x) -> element "internalApiErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (internalApiErrorfieldPathElements e) $ \x -> element "internalApiErrorfieldPathElements" $ toXML x
    case (internalApiErrortrigger e) of
        (Just x) -> element "internalApiErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (internalApiErrorerrorString e) of
        (Just x) -> element "internalApiErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (internalApiErrorApiErrorType e) of
        (Just x) -> element "internalApiErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML InternalApiError where
  parse = do
    _internalApiErrorfieldPath <- maybeElementContent "internalApiErrorfieldPath"
    _internalApiErrorfieldPathElements <- multipleElementContent "internalApiErrorfieldPathElements"
    _internalApiErrortrigger <- maybeElementContent "internalApiErrortrigger"
    _internalApiErrorerrorString <- maybeElementContent "internalApiErrorerrorString"
    _internalApiErrorApiErrorType <- maybeElementContent "internalApiErrorApiErrorType"
    return $ InternalApiError {
      internalApiErrorfieldPath = _internalApiErrorfieldPath
      , internalApiErrorfieldPathElements = _internalApiErrorfieldPathElements
      , internalApiErrortrigger = _internalApiErrortrigger
      , internalApiErrorerrorString = _internalApiErrorerrorString
      , internalApiErrorApiErrorType = _internalApiErrorApiErrorType
      }


data NotEmptyError = NotEmptyError { notEmptyErrorfieldPath :: Maybe Text
  , notEmptyErrorfieldPathElements :: [FieldPathElement]
  , notEmptyErrortrigger :: Maybe Text
  , notEmptyErrorerrorString :: Maybe Text
  , notEmptyErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default NotEmptyError where
  def = NotEmptyError { notEmptyErrorfieldPath = def, notEmptyErrorfieldPathElements = def, notEmptyErrortrigger = def, notEmptyErrorerrorString = def, notEmptyErrorApiErrorType = def }
instance ToXML NotEmptyError where
  toXML e = element "NotEmptyError" $ do
    case (notEmptyErrorfieldPath e) of
        (Just x) -> element "notEmptyErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (notEmptyErrorfieldPathElements e) $ \x -> element "notEmptyErrorfieldPathElements" $ toXML x
    case (notEmptyErrortrigger e) of
        (Just x) -> element "notEmptyErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (notEmptyErrorerrorString e) of
        (Just x) -> element "notEmptyErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (notEmptyErrorApiErrorType e) of
        (Just x) -> element "notEmptyErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML NotEmptyError where
  parse = do
    _notEmptyErrorfieldPath <- maybeElementContent "notEmptyErrorfieldPath"
    _notEmptyErrorfieldPathElements <- multipleElementContent "notEmptyErrorfieldPathElements"
    _notEmptyErrortrigger <- maybeElementContent "notEmptyErrortrigger"
    _notEmptyErrorerrorString <- maybeElementContent "notEmptyErrorerrorString"
    _notEmptyErrorApiErrorType <- maybeElementContent "notEmptyErrorApiErrorType"
    return $ NotEmptyError {
      notEmptyErrorfieldPath = _notEmptyErrorfieldPath
      , notEmptyErrorfieldPathElements = _notEmptyErrorfieldPathElements
      , notEmptyErrortrigger = _notEmptyErrortrigger
      , notEmptyErrorerrorString = _notEmptyErrorerrorString
      , notEmptyErrorApiErrorType = _notEmptyErrorApiErrorType
      }


data NullError = NullError { nullErrorfieldPath :: Maybe Text
  , nullErrorfieldPathElements :: [FieldPathElement]
  , nullErrortrigger :: Maybe Text
  , nullErrorerrorString :: Maybe Text
  , nullErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default NullError where
  def = NullError { nullErrorfieldPath = def, nullErrorfieldPathElements = def, nullErrortrigger = def, nullErrorerrorString = def, nullErrorApiErrorType = def }
instance ToXML NullError where
  toXML e = element "NullError" $ do
    case (nullErrorfieldPath e) of
        (Just x) -> element "nullErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (nullErrorfieldPathElements e) $ \x -> element "nullErrorfieldPathElements" $ toXML x
    case (nullErrortrigger e) of
        (Just x) -> element "nullErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (nullErrorerrorString e) of
        (Just x) -> element "nullErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (nullErrorApiErrorType e) of
        (Just x) -> element "nullErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML NullError where
  parse = do
    _nullErrorfieldPath <- maybeElementContent "nullErrorfieldPath"
    _nullErrorfieldPathElements <- multipleElementContent "nullErrorfieldPathElements"
    _nullErrortrigger <- maybeElementContent "nullErrortrigger"
    _nullErrorerrorString <- maybeElementContent "nullErrorerrorString"
    _nullErrorApiErrorType <- maybeElementContent "nullErrorApiErrorType"
    return $ NullError {
      nullErrorfieldPath = _nullErrorfieldPath
      , nullErrorfieldPathElements = _nullErrorfieldPathElements
      , nullErrortrigger = _nullErrortrigger
      , nullErrorerrorString = _nullErrorerrorString
      , nullErrorApiErrorType = _nullErrorApiErrorType
      }


data Operation = Operation { operationoperator :: Maybe Operator
  , operationOperationType :: Maybe Text } deriving (Show)
instance Default Operation where
  def = Operation { operationoperator = def, operationOperationType = def }
instance ToXML Operation where
  toXML e = element "Operation" $ do
    case (operationoperator e) of
        (Just x) -> element "operationoperator" $ toXML x
        (Nothing) -> content ""
    case (operationOperationType e) of
        (Just x) -> element "operationOperationType" $ toXML x
        (Nothing) -> content ""
instance FromXML Operation where
  parse = do
    _operationoperator <- maybeElementContent "operationoperator"
    _operationOperationType <- maybeElementContent "operationOperationType"
    return $ Operation {
      operationoperator = _operationoperator
      , operationOperationType = _operationOperationType
      }


data OperationAccessDenied = OperationAccessDenied { operationAccessDeniedfieldPath :: Maybe Text
  , operationAccessDeniedfieldPathElements :: [FieldPathElement]
  , operationAccessDeniedtrigger :: Maybe Text
  , operationAccessDeniederrorString :: Maybe Text
  , operationAccessDeniedApiErrorType :: Maybe Text } deriving (Show)
instance Default OperationAccessDenied where
  def = OperationAccessDenied { operationAccessDeniedfieldPath = def, operationAccessDeniedfieldPathElements = def, operationAccessDeniedtrigger = def, operationAccessDeniederrorString = def, operationAccessDeniedApiErrorType = def }
instance ToXML OperationAccessDenied where
  toXML e = element "OperationAccessDenied" $ do
    case (operationAccessDeniedfieldPath e) of
        (Just x) -> element "operationAccessDeniedfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (operationAccessDeniedfieldPathElements e) $ \x -> element "operationAccessDeniedfieldPathElements" $ toXML x
    case (operationAccessDeniedtrigger e) of
        (Just x) -> element "operationAccessDeniedtrigger" $ toXML x
        (Nothing) -> content ""
    case (operationAccessDeniederrorString e) of
        (Just x) -> element "operationAccessDeniederrorString" $ toXML x
        (Nothing) -> content ""
    case (operationAccessDeniedApiErrorType e) of
        (Just x) -> element "operationAccessDeniedApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML OperationAccessDenied where
  parse = do
    _operationAccessDeniedfieldPath <- maybeElementContent "operationAccessDeniedfieldPath"
    _operationAccessDeniedfieldPathElements <- multipleElementContent "operationAccessDeniedfieldPathElements"
    _operationAccessDeniedtrigger <- maybeElementContent "operationAccessDeniedtrigger"
    _operationAccessDeniederrorString <- maybeElementContent "operationAccessDeniederrorString"
    _operationAccessDeniedApiErrorType <- maybeElementContent "operationAccessDeniedApiErrorType"
    return $ OperationAccessDenied {
      operationAccessDeniedfieldPath = _operationAccessDeniedfieldPath
      , operationAccessDeniedfieldPathElements = _operationAccessDeniedfieldPathElements
      , operationAccessDeniedtrigger = _operationAccessDeniedtrigger
      , operationAccessDeniederrorString = _operationAccessDeniederrorString
      , operationAccessDeniedApiErrorType = _operationAccessDeniedApiErrorType
      }


data OperatorError = OperatorError { operatorErrorfieldPath :: Maybe Text
  , operatorErrorfieldPathElements :: [FieldPathElement]
  , operatorErrortrigger :: Maybe Text
  , operatorErrorerrorString :: Maybe Text
  , operatorErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default OperatorError where
  def = OperatorError { operatorErrorfieldPath = def, operatorErrorfieldPathElements = def, operatorErrortrigger = def, operatorErrorerrorString = def, operatorErrorApiErrorType = def }
instance ToXML OperatorError where
  toXML e = element "OperatorError" $ do
    case (operatorErrorfieldPath e) of
        (Just x) -> element "operatorErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (operatorErrorfieldPathElements e) $ \x -> element "operatorErrorfieldPathElements" $ toXML x
    case (operatorErrortrigger e) of
        (Just x) -> element "operatorErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (operatorErrorerrorString e) of
        (Just x) -> element "operatorErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (operatorErrorApiErrorType e) of
        (Just x) -> element "operatorErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML OperatorError where
  parse = do
    _operatorErrorfieldPath <- maybeElementContent "operatorErrorfieldPath"
    _operatorErrorfieldPathElements <- multipleElementContent "operatorErrorfieldPathElements"
    _operatorErrortrigger <- maybeElementContent "operatorErrortrigger"
    _operatorErrorerrorString <- maybeElementContent "operatorErrorerrorString"
    _operatorErrorApiErrorType <- maybeElementContent "operatorErrorApiErrorType"
    return $ OperatorError {
      operatorErrorfieldPath = _operatorErrorfieldPath
      , operatorErrorfieldPathElements = _operatorErrorfieldPathElements
      , operatorErrortrigger = _operatorErrortrigger
      , operatorErrorerrorString = _operatorErrorerrorString
      , operatorErrorApiErrorType = _operatorErrorApiErrorType
      }


data OrderBy = OrderBy { orderByfield :: Maybe Text
  , orderBysortOrder :: Maybe SortOrder } deriving (Show)
instance Default OrderBy where
  def = OrderBy { orderByfield = def, orderBysortOrder = def }
instance ToXML OrderBy where
  toXML e = element "OrderBy" $ do
    case (orderByfield e) of
        (Just x) -> element "orderByfield" $ toXML x
        (Nothing) -> content ""
    case (orderBysortOrder e) of
        (Just x) -> element "orderBysortOrder" $ toXML x
        (Nothing) -> content ""
instance FromXML OrderBy where
  parse = do
    _orderByfield <- maybeElementContent "orderByfield"
    _orderBysortOrder <- maybeElementContent "orderBysortOrder"
    return $ OrderBy {
      orderByfield = _orderByfield
      , orderBysortOrder = _orderBysortOrder
      }


data Paging = Paging { pagingstartIndex :: Maybe Int
  , pagingnumberResults :: Maybe Int } deriving (Show)
instance Default Paging where
  def = Paging { pagingstartIndex = def, pagingnumberResults = def }
instance ToXML Paging where
  toXML e = element "Paging" $ do
    case (pagingstartIndex e) of
        (Just x) -> element "pagingstartIndex" $ toXML x
        (Nothing) -> content ""
    case (pagingnumberResults e) of
        (Just x) -> element "pagingnumberResults" $ toXML x
        (Nothing) -> content ""
instance FromXML Paging where
  parse = do
    _pagingstartIndex <- maybeElementContent "pagingstartIndex"
    _pagingnumberResults <- maybeElementContent "pagingnumberResults"
    return $ Paging {
      pagingstartIndex = _pagingstartIndex
      , pagingnumberResults = _pagingnumberResults
      }


data Predicate = Predicate { predicatefield :: Maybe Text
  , predicateoperator :: Maybe PredicateOperator
  , predicatevalues :: [Text] } deriving (Show)
instance Default Predicate where
  def = Predicate { predicatefield = def, predicateoperator = def, predicatevalues = def }
instance ToXML Predicate where
  toXML e = element "Predicate" $ do
    case (predicatefield e) of
        (Just x) -> element "predicatefield" $ toXML x
        (Nothing) -> content ""
    case (predicateoperator e) of
        (Just x) -> element "predicateoperator" $ toXML x
        (Nothing) -> content ""
    forM_ (predicatevalues e) $ \x -> element "predicatevalues" $ toXML x
instance FromXML Predicate where
  parse = do
    _predicatefield <- maybeElementContent "predicatefield"
    _predicateoperator <- maybeElementContent "predicateoperator"
    _predicatevalues <- multipleElementContent "predicatevalues"
    return $ Predicate {
      predicatefield = _predicatefield
      , predicateoperator = _predicateoperator
      , predicatevalues = _predicatevalues
      }


data QuotaCheckError = QuotaCheckError { quotaCheckErrorfieldPath :: Maybe Text
  , quotaCheckErrorfieldPathElements :: [FieldPathElement]
  , quotaCheckErrortrigger :: Maybe Text
  , quotaCheckErrorerrorString :: Maybe Text
  , quotaCheckErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default QuotaCheckError where
  def = QuotaCheckError { quotaCheckErrorfieldPath = def, quotaCheckErrorfieldPathElements = def, quotaCheckErrortrigger = def, quotaCheckErrorerrorString = def, quotaCheckErrorApiErrorType = def }
instance ToXML QuotaCheckError where
  toXML e = element "QuotaCheckError" $ do
    case (quotaCheckErrorfieldPath e) of
        (Just x) -> element "quotaCheckErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (quotaCheckErrorfieldPathElements e) $ \x -> element "quotaCheckErrorfieldPathElements" $ toXML x
    case (quotaCheckErrortrigger e) of
        (Just x) -> element "quotaCheckErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (quotaCheckErrorerrorString e) of
        (Just x) -> element "quotaCheckErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (quotaCheckErrorApiErrorType e) of
        (Just x) -> element "quotaCheckErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML QuotaCheckError where
  parse = do
    _quotaCheckErrorfieldPath <- maybeElementContent "quotaCheckErrorfieldPath"
    _quotaCheckErrorfieldPathElements <- multipleElementContent "quotaCheckErrorfieldPathElements"
    _quotaCheckErrortrigger <- maybeElementContent "quotaCheckErrortrigger"
    _quotaCheckErrorerrorString <- maybeElementContent "quotaCheckErrorerrorString"
    _quotaCheckErrorApiErrorType <- maybeElementContent "quotaCheckErrorApiErrorType"
    return $ QuotaCheckError {
      quotaCheckErrorfieldPath = _quotaCheckErrorfieldPath
      , quotaCheckErrorfieldPathElements = _quotaCheckErrorfieldPathElements
      , quotaCheckErrortrigger = _quotaCheckErrortrigger
      , quotaCheckErrorerrorString = _quotaCheckErrorerrorString
      , quotaCheckErrorApiErrorType = _quotaCheckErrorApiErrorType
      }


data RangeError = RangeError { rangeErrorfieldPath :: Maybe Text
  , rangeErrorfieldPathElements :: [FieldPathElement]
  , rangeErrortrigger :: Maybe Text
  , rangeErrorerrorString :: Maybe Text
  , rangeErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default RangeError where
  def = RangeError { rangeErrorfieldPath = def, rangeErrorfieldPathElements = def, rangeErrortrigger = def, rangeErrorerrorString = def, rangeErrorApiErrorType = def }
instance ToXML RangeError where
  toXML e = element "RangeError" $ do
    case (rangeErrorfieldPath e) of
        (Just x) -> element "rangeErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (rangeErrorfieldPathElements e) $ \x -> element "rangeErrorfieldPathElements" $ toXML x
    case (rangeErrortrigger e) of
        (Just x) -> element "rangeErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (rangeErrorerrorString e) of
        (Just x) -> element "rangeErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (rangeErrorApiErrorType e) of
        (Just x) -> element "rangeErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML RangeError where
  parse = do
    _rangeErrorfieldPath <- maybeElementContent "rangeErrorfieldPath"
    _rangeErrorfieldPathElements <- multipleElementContent "rangeErrorfieldPathElements"
    _rangeErrortrigger <- maybeElementContent "rangeErrortrigger"
    _rangeErrorerrorString <- maybeElementContent "rangeErrorerrorString"
    _rangeErrorApiErrorType <- maybeElementContent "rangeErrorApiErrorType"
    return $ RangeError {
      rangeErrorfieldPath = _rangeErrorfieldPath
      , rangeErrorfieldPathElements = _rangeErrorfieldPathElements
      , rangeErrortrigger = _rangeErrortrigger
      , rangeErrorerrorString = _rangeErrorerrorString
      , rangeErrorApiErrorType = _rangeErrorApiErrorType
      }


data RateExceededError = RateExceededError { rateExceededErrorfieldPath :: Maybe Text
  , rateExceededErrorfieldPathElements :: [FieldPathElement]
  , rateExceededErrortrigger :: Maybe Text
  , rateExceededErrorerrorString :: Maybe Text
  , rateExceededErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default RateExceededError where
  def = RateExceededError { rateExceededErrorfieldPath = def, rateExceededErrorfieldPathElements = def, rateExceededErrortrigger = def, rateExceededErrorerrorString = def, rateExceededErrorApiErrorType = def }
instance ToXML RateExceededError where
  toXML e = element "RateExceededError" $ do
    case (rateExceededErrorfieldPath e) of
        (Just x) -> element "rateExceededErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (rateExceededErrorfieldPathElements e) $ \x -> element "rateExceededErrorfieldPathElements" $ toXML x
    case (rateExceededErrortrigger e) of
        (Just x) -> element "rateExceededErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (rateExceededErrorerrorString e) of
        (Just x) -> element "rateExceededErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (rateExceededErrorApiErrorType e) of
        (Just x) -> element "rateExceededErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML RateExceededError where
  parse = do
    _rateExceededErrorfieldPath <- maybeElementContent "rateExceededErrorfieldPath"
    _rateExceededErrorfieldPathElements <- multipleElementContent "rateExceededErrorfieldPathElements"
    _rateExceededErrortrigger <- maybeElementContent "rateExceededErrortrigger"
    _rateExceededErrorerrorString <- maybeElementContent "rateExceededErrorerrorString"
    _rateExceededErrorApiErrorType <- maybeElementContent "rateExceededErrorApiErrorType"
    return $ RateExceededError {
      rateExceededErrorfieldPath = _rateExceededErrorfieldPath
      , rateExceededErrorfieldPathElements = _rateExceededErrorfieldPathElements
      , rateExceededErrortrigger = _rateExceededErrortrigger
      , rateExceededErrorerrorString = _rateExceededErrorerrorString
      , rateExceededErrorApiErrorType = _rateExceededErrorApiErrorType
      }


data ReadOnlyError = ReadOnlyError { readOnlyErrorfieldPath :: Maybe Text
  , readOnlyErrorfieldPathElements :: [FieldPathElement]
  , readOnlyErrortrigger :: Maybe Text
  , readOnlyErrorerrorString :: Maybe Text
  , readOnlyErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default ReadOnlyError where
  def = ReadOnlyError { readOnlyErrorfieldPath = def, readOnlyErrorfieldPathElements = def, readOnlyErrortrigger = def, readOnlyErrorerrorString = def, readOnlyErrorApiErrorType = def }
instance ToXML ReadOnlyError where
  toXML e = element "ReadOnlyError" $ do
    case (readOnlyErrorfieldPath e) of
        (Just x) -> element "readOnlyErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (readOnlyErrorfieldPathElements e) $ \x -> element "readOnlyErrorfieldPathElements" $ toXML x
    case (readOnlyErrortrigger e) of
        (Just x) -> element "readOnlyErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (readOnlyErrorerrorString e) of
        (Just x) -> element "readOnlyErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (readOnlyErrorApiErrorType e) of
        (Just x) -> element "readOnlyErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML ReadOnlyError where
  parse = do
    _readOnlyErrorfieldPath <- maybeElementContent "readOnlyErrorfieldPath"
    _readOnlyErrorfieldPathElements <- multipleElementContent "readOnlyErrorfieldPathElements"
    _readOnlyErrortrigger <- maybeElementContent "readOnlyErrortrigger"
    _readOnlyErrorerrorString <- maybeElementContent "readOnlyErrorerrorString"
    _readOnlyErrorApiErrorType <- maybeElementContent "readOnlyErrorApiErrorType"
    return $ ReadOnlyError {
      readOnlyErrorfieldPath = _readOnlyErrorfieldPath
      , readOnlyErrorfieldPathElements = _readOnlyErrorfieldPathElements
      , readOnlyErrortrigger = _readOnlyErrortrigger
      , readOnlyErrorerrorString = _readOnlyErrorerrorString
      , readOnlyErrorApiErrorType = _readOnlyErrorApiErrorType
      }


data RegionCodeError = RegionCodeError { regionCodeErrorfieldPath :: Maybe Text
  , regionCodeErrorfieldPathElements :: [FieldPathElement]
  , regionCodeErrortrigger :: Maybe Text
  , regionCodeErrorerrorString :: Maybe Text
  , regionCodeErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default RegionCodeError where
  def = RegionCodeError { regionCodeErrorfieldPath = def, regionCodeErrorfieldPathElements = def, regionCodeErrortrigger = def, regionCodeErrorerrorString = def, regionCodeErrorApiErrorType = def }
instance ToXML RegionCodeError where
  toXML e = element "RegionCodeError" $ do
    case (regionCodeErrorfieldPath e) of
        (Just x) -> element "regionCodeErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (regionCodeErrorfieldPathElements e) $ \x -> element "regionCodeErrorfieldPathElements" $ toXML x
    case (regionCodeErrortrigger e) of
        (Just x) -> element "regionCodeErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (regionCodeErrorerrorString e) of
        (Just x) -> element "regionCodeErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (regionCodeErrorApiErrorType e) of
        (Just x) -> element "regionCodeErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML RegionCodeError where
  parse = do
    _regionCodeErrorfieldPath <- maybeElementContent "regionCodeErrorfieldPath"
    _regionCodeErrorfieldPathElements <- multipleElementContent "regionCodeErrorfieldPathElements"
    _regionCodeErrortrigger <- maybeElementContent "regionCodeErrortrigger"
    _regionCodeErrorerrorString <- maybeElementContent "regionCodeErrorerrorString"
    _regionCodeErrorApiErrorType <- maybeElementContent "regionCodeErrorApiErrorType"
    return $ RegionCodeError {
      regionCodeErrorfieldPath = _regionCodeErrorfieldPath
      , regionCodeErrorfieldPathElements = _regionCodeErrorfieldPathElements
      , regionCodeErrortrigger = _regionCodeErrortrigger
      , regionCodeErrorerrorString = _regionCodeErrorerrorString
      , regionCodeErrorApiErrorType = _regionCodeErrorApiErrorType
      }


data RejectedError = RejectedError { rejectedErrorfieldPath :: Maybe Text
  , rejectedErrorfieldPathElements :: [FieldPathElement]
  , rejectedErrortrigger :: Maybe Text
  , rejectedErrorerrorString :: Maybe Text
  , rejectedErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default RejectedError where
  def = RejectedError { rejectedErrorfieldPath = def, rejectedErrorfieldPathElements = def, rejectedErrortrigger = def, rejectedErrorerrorString = def, rejectedErrorApiErrorType = def }
instance ToXML RejectedError where
  toXML e = element "RejectedError" $ do
    case (rejectedErrorfieldPath e) of
        (Just x) -> element "rejectedErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (rejectedErrorfieldPathElements e) $ \x -> element "rejectedErrorfieldPathElements" $ toXML x
    case (rejectedErrortrigger e) of
        (Just x) -> element "rejectedErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (rejectedErrorerrorString e) of
        (Just x) -> element "rejectedErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (rejectedErrorApiErrorType e) of
        (Just x) -> element "rejectedErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML RejectedError where
  parse = do
    _rejectedErrorfieldPath <- maybeElementContent "rejectedErrorfieldPath"
    _rejectedErrorfieldPathElements <- multipleElementContent "rejectedErrorfieldPathElements"
    _rejectedErrortrigger <- maybeElementContent "rejectedErrortrigger"
    _rejectedErrorerrorString <- maybeElementContent "rejectedErrorerrorString"
    _rejectedErrorApiErrorType <- maybeElementContent "rejectedErrorApiErrorType"
    return $ RejectedError {
      rejectedErrorfieldPath = _rejectedErrorfieldPath
      , rejectedErrorfieldPathElements = _rejectedErrorfieldPathElements
      , rejectedErrortrigger = _rejectedErrortrigger
      , rejectedErrorerrorString = _rejectedErrorerrorString
      , rejectedErrorApiErrorType = _rejectedErrorApiErrorType
      }


data RequestError = RequestError { requestErrorfieldPath :: Maybe Text
  , requestErrorfieldPathElements :: [FieldPathElement]
  , requestErrortrigger :: Maybe Text
  , requestErrorerrorString :: Maybe Text
  , requestErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default RequestError where
  def = RequestError { requestErrorfieldPath = def, requestErrorfieldPathElements = def, requestErrortrigger = def, requestErrorerrorString = def, requestErrorApiErrorType = def }
instance ToXML RequestError where
  toXML e = element "RequestError" $ do
    case (requestErrorfieldPath e) of
        (Just x) -> element "requestErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (requestErrorfieldPathElements e) $ \x -> element "requestErrorfieldPathElements" $ toXML x
    case (requestErrortrigger e) of
        (Just x) -> element "requestErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (requestErrorerrorString e) of
        (Just x) -> element "requestErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (requestErrorApiErrorType e) of
        (Just x) -> element "requestErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML RequestError where
  parse = do
    _requestErrorfieldPath <- maybeElementContent "requestErrorfieldPath"
    _requestErrorfieldPathElements <- multipleElementContent "requestErrorfieldPathElements"
    _requestErrortrigger <- maybeElementContent "requestErrortrigger"
    _requestErrorerrorString <- maybeElementContent "requestErrorerrorString"
    _requestErrorApiErrorType <- maybeElementContent "requestErrorApiErrorType"
    return $ RequestError {
      requestErrorfieldPath = _requestErrorfieldPath
      , requestErrorfieldPathElements = _requestErrorfieldPathElements
      , requestErrortrigger = _requestErrortrigger
      , requestErrorerrorString = _requestErrorerrorString
      , requestErrorApiErrorType = _requestErrorApiErrorType
      }


data RequiredError = RequiredError { requiredErrorfieldPath :: Maybe Text
  , requiredErrorfieldPathElements :: [FieldPathElement]
  , requiredErrortrigger :: Maybe Text
  , requiredErrorerrorString :: Maybe Text
  , requiredErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default RequiredError where
  def = RequiredError { requiredErrorfieldPath = def, requiredErrorfieldPathElements = def, requiredErrortrigger = def, requiredErrorerrorString = def, requiredErrorApiErrorType = def }
instance ToXML RequiredError where
  toXML e = element "RequiredError" $ do
    case (requiredErrorfieldPath e) of
        (Just x) -> element "requiredErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (requiredErrorfieldPathElements e) $ \x -> element "requiredErrorfieldPathElements" $ toXML x
    case (requiredErrortrigger e) of
        (Just x) -> element "requiredErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (requiredErrorerrorString e) of
        (Just x) -> element "requiredErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (requiredErrorApiErrorType e) of
        (Just x) -> element "requiredErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML RequiredError where
  parse = do
    _requiredErrorfieldPath <- maybeElementContent "requiredErrorfieldPath"
    _requiredErrorfieldPathElements <- multipleElementContent "requiredErrorfieldPathElements"
    _requiredErrortrigger <- maybeElementContent "requiredErrortrigger"
    _requiredErrorerrorString <- maybeElementContent "requiredErrorerrorString"
    _requiredErrorApiErrorType <- maybeElementContent "requiredErrorApiErrorType"
    return $ RequiredError {
      requiredErrorfieldPath = _requiredErrorfieldPath
      , requiredErrorfieldPathElements = _requiredErrorfieldPathElements
      , requiredErrortrigger = _requiredErrortrigger
      , requiredErrorerrorString = _requiredErrorerrorString
      , requiredErrorApiErrorType = _requiredErrorApiErrorType
      }


data Selector = Selector { selectorfields :: [Text]
  , selectorpredicates :: [Predicate]
  , selectordateRange :: Maybe DateRange
  , selectorordering :: [OrderBy]
  , selectorpaging :: Maybe Paging } deriving (Show)
instance Default Selector where
  def = Selector { selectorfields = def, selectorpredicates = def, selectordateRange = def, selectorordering = def, selectorpaging = def }
instance ToXML Selector where
  toXML e = element "Selector" $ do
    forM_ (selectorfields e) $ \x -> element "selectorfields" $ toXML x
    forM_ (selectorpredicates e) $ \x -> element "selectorpredicates" $ toXML x
    case (selectordateRange e) of
        (Just x) -> element "selectordateRange" $ toXML x
        (Nothing) -> content ""
    forM_ (selectorordering e) $ \x -> element "selectorordering" $ toXML x
    case (selectorpaging e) of
        (Just x) -> element "selectorpaging" $ toXML x
        (Nothing) -> content ""
instance FromXML Selector where
  parse = do
    _selectorfields <- multipleElementContent "selectorfields"
    _selectorpredicates <- multipleElementContent "selectorpredicates"
    _selectordateRange <- maybeElementContent "selectordateRange"
    _selectorordering <- multipleElementContent "selectorordering"
    _selectorpaging <- maybeElementContent "selectorpaging"
    return $ Selector {
      selectorfields = _selectorfields
      , selectorpredicates = _selectorpredicates
      , selectordateRange = _selectordateRange
      , selectorordering = _selectorordering
      , selectorpaging = _selectorpaging
      }


data SelectorError = SelectorError { selectorErrorfieldPath :: Maybe Text
  , selectorErrorfieldPathElements :: [FieldPathElement]
  , selectorErrortrigger :: Maybe Text
  , selectorErrorerrorString :: Maybe Text
  , selectorErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default SelectorError where
  def = SelectorError { selectorErrorfieldPath = def, selectorErrorfieldPathElements = def, selectorErrortrigger = def, selectorErrorerrorString = def, selectorErrorApiErrorType = def }
instance ToXML SelectorError where
  toXML e = element "SelectorError" $ do
    case (selectorErrorfieldPath e) of
        (Just x) -> element "selectorErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (selectorErrorfieldPathElements e) $ \x -> element "selectorErrorfieldPathElements" $ toXML x
    case (selectorErrortrigger e) of
        (Just x) -> element "selectorErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (selectorErrorerrorString e) of
        (Just x) -> element "selectorErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (selectorErrorApiErrorType e) of
        (Just x) -> element "selectorErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML SelectorError where
  parse = do
    _selectorErrorfieldPath <- maybeElementContent "selectorErrorfieldPath"
    _selectorErrorfieldPathElements <- multipleElementContent "selectorErrorfieldPathElements"
    _selectorErrortrigger <- maybeElementContent "selectorErrortrigger"
    _selectorErrorerrorString <- maybeElementContent "selectorErrorerrorString"
    _selectorErrorApiErrorType <- maybeElementContent "selectorErrorApiErrorType"
    return $ SelectorError {
      selectorErrorfieldPath = _selectorErrorfieldPath
      , selectorErrorfieldPathElements = _selectorErrorfieldPathElements
      , selectorErrortrigger = _selectorErrortrigger
      , selectorErrorerrorString = _selectorErrorerrorString
      , selectorErrorApiErrorType = _selectorErrorApiErrorType
      }


data SizeLimitError = SizeLimitError { sizeLimitErrorfieldPath :: Maybe Text
  , sizeLimitErrorfieldPathElements :: [FieldPathElement]
  , sizeLimitErrortrigger :: Maybe Text
  , sizeLimitErrorerrorString :: Maybe Text
  , sizeLimitErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default SizeLimitError where
  def = SizeLimitError { sizeLimitErrorfieldPath = def, sizeLimitErrorfieldPathElements = def, sizeLimitErrortrigger = def, sizeLimitErrorerrorString = def, sizeLimitErrorApiErrorType = def }
instance ToXML SizeLimitError where
  toXML e = element "SizeLimitError" $ do
    case (sizeLimitErrorfieldPath e) of
        (Just x) -> element "sizeLimitErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (sizeLimitErrorfieldPathElements e) $ \x -> element "sizeLimitErrorfieldPathElements" $ toXML x
    case (sizeLimitErrortrigger e) of
        (Just x) -> element "sizeLimitErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (sizeLimitErrorerrorString e) of
        (Just x) -> element "sizeLimitErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (sizeLimitErrorApiErrorType e) of
        (Just x) -> element "sizeLimitErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML SizeLimitError where
  parse = do
    _sizeLimitErrorfieldPath <- maybeElementContent "sizeLimitErrorfieldPath"
    _sizeLimitErrorfieldPathElements <- multipleElementContent "sizeLimitErrorfieldPathElements"
    _sizeLimitErrortrigger <- maybeElementContent "sizeLimitErrortrigger"
    _sizeLimitErrorerrorString <- maybeElementContent "sizeLimitErrorerrorString"
    _sizeLimitErrorApiErrorType <- maybeElementContent "sizeLimitErrorApiErrorType"
    return $ SizeLimitError {
      sizeLimitErrorfieldPath = _sizeLimitErrorfieldPath
      , sizeLimitErrorfieldPathElements = _sizeLimitErrorfieldPathElements
      , sizeLimitErrortrigger = _sizeLimitErrortrigger
      , sizeLimitErrorerrorString = _sizeLimitErrorerrorString
      , sizeLimitErrorApiErrorType = _sizeLimitErrorApiErrorType
      }


data SoapHeader = SoapHeader { soapHeaderclientCustomerId :: Maybe Text
  , soapHeaderdeveloperToken :: Maybe Text
  , soapHeaderuserAgent :: Maybe Text
  , soapHeadervalidateOnly :: Maybe Bool
  , soapHeaderpartialFailure :: Maybe Bool } deriving (Show)
instance Default SoapHeader where
  def = SoapHeader { soapHeaderclientCustomerId = def, soapHeaderdeveloperToken = def, soapHeaderuserAgent = def, soapHeadervalidateOnly = def, soapHeaderpartialFailure = def }
instance ToXML SoapHeader where
  toXML e = element "SoapHeader" $ do
    case (soapHeaderclientCustomerId e) of
        (Just x) -> element "soapHeaderclientCustomerId" $ toXML x
        (Nothing) -> content ""
    case (soapHeaderdeveloperToken e) of
        (Just x) -> element "soapHeaderdeveloperToken" $ toXML x
        (Nothing) -> content ""
    case (soapHeaderuserAgent e) of
        (Just x) -> element "soapHeaderuserAgent" $ toXML x
        (Nothing) -> content ""
    case (soapHeadervalidateOnly e) of
        (Just x) -> element "soapHeadervalidateOnly" $ toXML x
        (Nothing) -> content ""
    case (soapHeaderpartialFailure e) of
        (Just x) -> element "soapHeaderpartialFailure" $ toXML x
        (Nothing) -> content ""
instance FromXML SoapHeader where
  parse = do
    _soapHeaderclientCustomerId <- maybeElementContent "soapHeaderclientCustomerId"
    _soapHeaderdeveloperToken <- maybeElementContent "soapHeaderdeveloperToken"
    _soapHeaderuserAgent <- maybeElementContent "soapHeaderuserAgent"
    _soapHeadervalidateOnly <- maybeElementContent "soapHeadervalidateOnly"
    _soapHeaderpartialFailure <- maybeElementContent "soapHeaderpartialFailure"
    return $ SoapHeader {
      soapHeaderclientCustomerId = _soapHeaderclientCustomerId
      , soapHeaderdeveloperToken = _soapHeaderdeveloperToken
      , soapHeaderuserAgent = _soapHeaderuserAgent
      , soapHeadervalidateOnly = _soapHeadervalidateOnly
      , soapHeaderpartialFailure = _soapHeaderpartialFailure
      }


data SoapResponseHeader = SoapResponseHeader { soapResponseHeaderrequestId :: Maybe Text
  , soapResponseHeaderserviceName :: Maybe Text
  , soapResponseHeadermethodName :: Maybe Text
  , soapResponseHeaderoperations :: Maybe Int
  , soapResponseHeaderresponseTime :: Maybe Int } deriving (Show)
instance Default SoapResponseHeader where
  def = SoapResponseHeader { soapResponseHeaderrequestId = def, soapResponseHeaderserviceName = def, soapResponseHeadermethodName = def, soapResponseHeaderoperations = def, soapResponseHeaderresponseTime = def }
instance ToXML SoapResponseHeader where
  toXML e = element "SoapResponseHeader" $ do
    case (soapResponseHeaderrequestId e) of
        (Just x) -> element "soapResponseHeaderrequestId" $ toXML x
        (Nothing) -> content ""
    case (soapResponseHeaderserviceName e) of
        (Just x) -> element "soapResponseHeaderserviceName" $ toXML x
        (Nothing) -> content ""
    case (soapResponseHeadermethodName e) of
        (Just x) -> element "soapResponseHeadermethodName" $ toXML x
        (Nothing) -> content ""
    case (soapResponseHeaderoperations e) of
        (Just x) -> element "soapResponseHeaderoperations" $ toXML x
        (Nothing) -> content ""
    case (soapResponseHeaderresponseTime e) of
        (Just x) -> element "soapResponseHeaderresponseTime" $ toXML x
        (Nothing) -> content ""
instance FromXML SoapResponseHeader where
  parse = do
    _soapResponseHeaderrequestId <- maybeElementContent "soapResponseHeaderrequestId"
    _soapResponseHeaderserviceName <- maybeElementContent "soapResponseHeaderserviceName"
    _soapResponseHeadermethodName <- maybeElementContent "soapResponseHeadermethodName"
    _soapResponseHeaderoperations <- maybeElementContent "soapResponseHeaderoperations"
    _soapResponseHeaderresponseTime <- maybeElementContent "soapResponseHeaderresponseTime"
    return $ SoapResponseHeader {
      soapResponseHeaderrequestId = _soapResponseHeaderrequestId
      , soapResponseHeaderserviceName = _soapResponseHeaderserviceName
      , soapResponseHeadermethodName = _soapResponseHeadermethodName
      , soapResponseHeaderoperations = _soapResponseHeaderoperations
      , soapResponseHeaderresponseTime = _soapResponseHeaderresponseTime
      }


data StringFormatError = StringFormatError { stringFormatErrorfieldPath :: Maybe Text
  , stringFormatErrorfieldPathElements :: [FieldPathElement]
  , stringFormatErrortrigger :: Maybe Text
  , stringFormatErrorerrorString :: Maybe Text
  , stringFormatErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default StringFormatError where
  def = StringFormatError { stringFormatErrorfieldPath = def, stringFormatErrorfieldPathElements = def, stringFormatErrortrigger = def, stringFormatErrorerrorString = def, stringFormatErrorApiErrorType = def }
instance ToXML StringFormatError where
  toXML e = element "StringFormatError" $ do
    case (stringFormatErrorfieldPath e) of
        (Just x) -> element "stringFormatErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (stringFormatErrorfieldPathElements e) $ \x -> element "stringFormatErrorfieldPathElements" $ toXML x
    case (stringFormatErrortrigger e) of
        (Just x) -> element "stringFormatErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (stringFormatErrorerrorString e) of
        (Just x) -> element "stringFormatErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (stringFormatErrorApiErrorType e) of
        (Just x) -> element "stringFormatErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML StringFormatError where
  parse = do
    _stringFormatErrorfieldPath <- maybeElementContent "stringFormatErrorfieldPath"
    _stringFormatErrorfieldPathElements <- multipleElementContent "stringFormatErrorfieldPathElements"
    _stringFormatErrortrigger <- maybeElementContent "stringFormatErrortrigger"
    _stringFormatErrorerrorString <- maybeElementContent "stringFormatErrorerrorString"
    _stringFormatErrorApiErrorType <- maybeElementContent "stringFormatErrorApiErrorType"
    return $ StringFormatError {
      stringFormatErrorfieldPath = _stringFormatErrorfieldPath
      , stringFormatErrorfieldPathElements = _stringFormatErrorfieldPathElements
      , stringFormatErrortrigger = _stringFormatErrortrigger
      , stringFormatErrorerrorString = _stringFormatErrorerrorString
      , stringFormatErrorApiErrorType = _stringFormatErrorApiErrorType
      }


data StringLengthError = StringLengthError { stringLengthErrorfieldPath :: Maybe Text
  , stringLengthErrorfieldPathElements :: [FieldPathElement]
  , stringLengthErrortrigger :: Maybe Text
  , stringLengthErrorerrorString :: Maybe Text
  , stringLengthErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default StringLengthError where
  def = StringLengthError { stringLengthErrorfieldPath = def, stringLengthErrorfieldPathElements = def, stringLengthErrortrigger = def, stringLengthErrorerrorString = def, stringLengthErrorApiErrorType = def }
instance ToXML StringLengthError where
  toXML e = element "StringLengthError" $ do
    case (stringLengthErrorfieldPath e) of
        (Just x) -> element "stringLengthErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (stringLengthErrorfieldPathElements e) $ \x -> element "stringLengthErrorfieldPathElements" $ toXML x
    case (stringLengthErrortrigger e) of
        (Just x) -> element "stringLengthErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (stringLengthErrorerrorString e) of
        (Just x) -> element "stringLengthErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (stringLengthErrorApiErrorType e) of
        (Just x) -> element "stringLengthErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML StringLengthError where
  parse = do
    _stringLengthErrorfieldPath <- maybeElementContent "stringLengthErrorfieldPath"
    _stringLengthErrorfieldPathElements <- multipleElementContent "stringLengthErrorfieldPathElements"
    _stringLengthErrortrigger <- maybeElementContent "stringLengthErrortrigger"
    _stringLengthErrorerrorString <- maybeElementContent "stringLengthErrorerrorString"
    _stringLengthErrorApiErrorType <- maybeElementContent "stringLengthErrorApiErrorType"
    return $ StringLengthError {
      stringLengthErrorfieldPath = _stringLengthErrorfieldPath
      , stringLengthErrorfieldPathElements = _stringLengthErrorfieldPathElements
      , stringLengthErrortrigger = _stringLengthErrortrigger
      , stringLengthErrorerrorString = _stringLengthErrorerrorString
      , stringLengthErrorApiErrorType = _stringLengthErrorApiErrorType
      }
type AuthenticationErrorReason = Text
type AuthorizationErrorReason = Text
type ClientTermsErrorReason = Text
type CollectionSizeErrorReason = Text
type DatabaseErrorReason = Text
type DateErrorReason = Text
type DistinctErrorReason = Text
type IdErrorReason = Text
type InternalApiErrorReason = Text
type NotEmptyErrorReason = Text
type NullErrorReason = Text
type OperationAccessDeniedReason = Text
type Operator = Text
type OperatorErrorReason = Text
type PredicateOperator = Text
type QuotaCheckErrorReason = Text
type RangeErrorReason = Text
type RateExceededErrorReason = Text
type ReadOnlyErrorReason = Text
type RegionCodeErrorReason = Text
type RejectedErrorReason = Text
type RequestErrorReason = Text
type RequiredErrorReason = Text
type SelectorErrorReason = Text
type SizeLimitErrorReason = Text
type SortOrder = Text
type StringFormatErrorReason = Text
type StringLengthErrorReason = Text


data AccountLabelPage = AccountLabelPage { accountLabelPagelabels :: [AccountLabel] } deriving (Show)
instance Default AccountLabelPage where
  def = AccountLabelPage { accountLabelPagelabels = def }
instance ToXML AccountLabelPage where
  toXML e = element "AccountLabelPage" $ do
    forM_ (accountLabelPagelabels e) $ \x -> element "accountLabelPagelabels" $ toXML x
instance FromXML AccountLabelPage where
  parse = do
    _accountLabelPagelabels <- multipleElementContent "accountLabelPagelabels"
    return $ AccountLabelPage {
      accountLabelPagelabels = _accountLabelPagelabels
      }


data AccountLabelReturnValue = AccountLabelReturnValue { accountLabelReturnValuelabels :: [AccountLabel] } deriving (Show)
instance Default AccountLabelReturnValue where
  def = AccountLabelReturnValue { accountLabelReturnValuelabels = def }
instance ToXML AccountLabelReturnValue where
  toXML e = element "AccountLabelReturnValue" $ do
    forM_ (accountLabelReturnValuelabels e) $ \x -> element "accountLabelReturnValuelabels" $ toXML x
instance FromXML AccountLabelReturnValue where
  parse = do
    _accountLabelReturnValuelabels <- multipleElementContent "accountLabelReturnValuelabels"
    return $ AccountLabelReturnValue {
      accountLabelReturnValuelabels = _accountLabelReturnValuelabels
      }


data CurrencyCodeError = CurrencyCodeError { currencyCodeErrorfieldPath :: Maybe Text
  , currencyCodeErrorfieldPathElements :: [FieldPathElement]
  , currencyCodeErrortrigger :: Maybe Text
  , currencyCodeErrorerrorString :: Maybe Text
  , currencyCodeErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default CurrencyCodeError where
  def = CurrencyCodeError { currencyCodeErrorfieldPath = def, currencyCodeErrorfieldPathElements = def, currencyCodeErrortrigger = def, currencyCodeErrorerrorString = def, currencyCodeErrorApiErrorType = def }
instance ToXML CurrencyCodeError where
  toXML e = element "CurrencyCodeError" $ do
    case (currencyCodeErrorfieldPath e) of
        (Just x) -> element "currencyCodeErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (currencyCodeErrorfieldPathElements e) $ \x -> element "currencyCodeErrorfieldPathElements" $ toXML x
    case (currencyCodeErrortrigger e) of
        (Just x) -> element "currencyCodeErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (currencyCodeErrorerrorString e) of
        (Just x) -> element "currencyCodeErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (currencyCodeErrorApiErrorType e) of
        (Just x) -> element "currencyCodeErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML CurrencyCodeError where
  parse = do
    _currencyCodeErrorfieldPath <- maybeElementContent "currencyCodeErrorfieldPath"
    _currencyCodeErrorfieldPathElements <- multipleElementContent "currencyCodeErrorfieldPathElements"
    _currencyCodeErrortrigger <- maybeElementContent "currencyCodeErrortrigger"
    _currencyCodeErrorerrorString <- maybeElementContent "currencyCodeErrorerrorString"
    _currencyCodeErrorApiErrorType <- maybeElementContent "currencyCodeErrorApiErrorType"
    return $ CurrencyCodeError {
      currencyCodeErrorfieldPath = _currencyCodeErrorfieldPath
      , currencyCodeErrorfieldPathElements = _currencyCodeErrorfieldPathElements
      , currencyCodeErrortrigger = _currencyCodeErrortrigger
      , currencyCodeErrorerrorString = _currencyCodeErrorerrorString
      , currencyCodeErrorApiErrorType = _currencyCodeErrorApiErrorType
      }


data AccountLabel = AccountLabel { accountLabelid :: Maybe Int
  , accountLabelname :: Maybe Text } deriving (Show)
instance Default AccountLabel where
  def = AccountLabel { accountLabelid = def, accountLabelname = def }
instance ToXML AccountLabel where
  toXML e = element "AccountLabel" $ do
    case (accountLabelid e) of
        (Just x) -> element "accountLabelid" $ toXML x
        (Nothing) -> content ""
    case (accountLabelname e) of
        (Just x) -> element "accountLabelname" $ toXML x
        (Nothing) -> content ""
instance FromXML AccountLabel where
  parse = do
    _accountLabelid <- maybeElementContent "accountLabelid"
    _accountLabelname <- maybeElementContent "accountLabelname"
    return $ AccountLabel {
      accountLabelid = _accountLabelid
      , accountLabelname = _accountLabelname
      }


data LabelServiceError = LabelServiceError { labelServiceErrorfieldPath :: Maybe Text
  , labelServiceErrorfieldPathElements :: [FieldPathElement]
  , labelServiceErrortrigger :: Maybe Text
  , labelServiceErrorerrorString :: Maybe Text
  , labelServiceErrorApiErrorType :: Maybe Text } deriving (Show)
instance Default LabelServiceError where
  def = LabelServiceError { labelServiceErrorfieldPath = def, labelServiceErrorfieldPathElements = def, labelServiceErrortrigger = def, labelServiceErrorerrorString = def, labelServiceErrorApiErrorType = def }
instance ToXML LabelServiceError where
  toXML e = element "LabelServiceError" $ do
    case (labelServiceErrorfieldPath e) of
        (Just x) -> element "labelServiceErrorfieldPath" $ toXML x
        (Nothing) -> content ""
    forM_ (labelServiceErrorfieldPathElements e) $ \x -> element "labelServiceErrorfieldPathElements" $ toXML x
    case (labelServiceErrortrigger e) of
        (Just x) -> element "labelServiceErrortrigger" $ toXML x
        (Nothing) -> content ""
    case (labelServiceErrorerrorString e) of
        (Just x) -> element "labelServiceErrorerrorString" $ toXML x
        (Nothing) -> content ""
    case (labelServiceErrorApiErrorType e) of
        (Just x) -> element "labelServiceErrorApiErrorType" $ toXML x
        (Nothing) -> content ""
instance FromXML LabelServiceError where
  parse = do
    _labelServiceErrorfieldPath <- maybeElementContent "labelServiceErrorfieldPath"
    _labelServiceErrorfieldPathElements <- multipleElementContent "labelServiceErrorfieldPathElements"
    _labelServiceErrortrigger <- maybeElementContent "labelServiceErrortrigger"
    _labelServiceErrorerrorString <- maybeElementContent "labelServiceErrorerrorString"
    _labelServiceErrorApiErrorType <- maybeElementContent "labelServiceErrorApiErrorType"
    return $ LabelServiceError {
      labelServiceErrorfieldPath = _labelServiceErrorfieldPath
      , labelServiceErrorfieldPathElements = _labelServiceErrorfieldPathElements
      , labelServiceErrortrigger = _labelServiceErrortrigger
      , labelServiceErrorerrorString = _labelServiceErrorerrorString
      , labelServiceErrorApiErrorType = _labelServiceErrorApiErrorType
      }


data AccountLabelOperation = AccountLabelOperation { accountLabelOperationoperator :: Maybe Operator
  , accountLabelOperationOperationType :: Maybe Text } deriving (Show)
instance Default AccountLabelOperation where
  def = AccountLabelOperation { accountLabelOperationoperator = def, accountLabelOperationOperationType = def }
instance ToXML AccountLabelOperation where
  toXML e = element "AccountLabelOperation" $ do
    case (accountLabelOperationoperator e) of
        (Just x) -> element "accountLabelOperationoperator" $ toXML x
        (Nothing) -> content ""
    case (accountLabelOperationOperationType e) of
        (Just x) -> element "accountLabelOperationOperationType" $ toXML x
        (Nothing) -> content ""
instance FromXML AccountLabelOperation where
  parse = do
    _accountLabelOperationoperator <- maybeElementContent "accountLabelOperationoperator"
    _accountLabelOperationOperationType <- maybeElementContent "accountLabelOperationOperationType"
    return $ AccountLabelOperation {
      accountLabelOperationoperator = _accountLabelOperationoperator
      , accountLabelOperationOperationType = _accountLabelOperationOperationType
      }
type CurrencyCodeErrorReason = Text
type LabelServiceErrorReason = Text

