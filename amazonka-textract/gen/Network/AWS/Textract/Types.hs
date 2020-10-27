{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Textract.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Textract.Types
    (
    -- * Service Configuration
      textract

    -- * Errors
    , _InvalidJobIdException
    , _AccessDeniedException
    , _BadDocumentException
    , _InvalidParameterException
    , _UnsupportedDocumentException
    , _InvalidS3ObjectException
    , _ProvisionedThroughputExceededException
    , _ThrottlingException
    , _InternalServerError
    , _IdempotentParameterMismatchException
    , _HumanLoopQuotaExceededException
    , _DocumentTooLargeException
    , _LimitExceededException

    -- * BlockType
    , BlockType (..)

    -- * ContentClassifier
    , ContentClassifier (..)

    -- * EntityType
    , EntityType (..)

    -- * FeatureType
    , FeatureType (..)

    -- * JobStatus
    , JobStatus (..)

    -- * RelationshipType
    , RelationshipType (..)

    -- * SelectionStatus
    , SelectionStatus (..)

    -- * Block
    , Block
    , block
    , bColumnSpan
    , bText
    , bEntityTypes
    , bColumnIndex
    , bPage
    , bRowSpan
    , bSelectionStatus
    , bRowIndex
    , bConfidence
    , bRelationships
    , bGeometry
    , bId
    , bBlockType

    -- * BoundingBox
    , BoundingBox
    , boundingBox
    , bbHeight
    , bbLeft
    , bbWidth
    , bbTop

    -- * Document
    , Document
    , document
    , dS3Object
    , dBytes

    -- * DocumentLocation
    , DocumentLocation
    , documentLocation
    , dlS3Object

    -- * DocumentMetadata
    , DocumentMetadata
    , documentMetadata
    , dmPages

    -- * Geometry
    , Geometry
    , geometry
    , gBoundingBox
    , gPolygon

    -- * HumanLoopActivationOutput
    , HumanLoopActivationOutput
    , humanLoopActivationOutput
    , hlaoHumanLoopActivationReasons
    , hlaoHumanLoopARN
    , hlaoHumanLoopActivationConditionsEvaluationResults

    -- * HumanLoopConfig
    , HumanLoopConfig
    , humanLoopConfig
    , hlcDataAttributes
    , hlcHumanLoopName
    , hlcFlowDefinitionARN

    -- * HumanLoopDataAttributes
    , HumanLoopDataAttributes
    , humanLoopDataAttributes
    , hldaContentClassifiers

    -- * NotificationChannel
    , NotificationChannel
    , notificationChannel
    , ncSNSTopicARN
    , ncRoleARN

    -- * OutputConfig
    , OutputConfig
    , outputConfig
    , ocS3Prefix
    , ocS3Bucket

    -- * Point
    , Point
    , point
    , pX
    , pY

    -- * Relationship
    , Relationship
    , relationship
    , rIds
    , rType

    -- * S3Object
    , S3Object
    , s3Object
    , soBucket
    , soName
    , soVersion

    -- * Warning
    , Warning
    , warning
    , wPages
    , wErrorCode
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Textract.Types.Product
import Network.AWS.Textract.Types.Sum

-- | API version @2018-06-27@ of the Amazon Textract SDK configuration.
textract :: Service
textract =
  Service
    { _svcAbbrev = "Textract"
    , _svcSigner = v4
    , _svcPrefix = "textract"
    , _svcVersion = "2018-06-27"
    , _svcEndpoint = defaultEndpoint textract
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Textract"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasCode "ProvisionedThroughputExceededException" . hasStatus 400) e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | An invalid job identifier was passed to 'GetDocumentAnalysis' or to 'GetDocumentAnalysis' .
--
--
_InvalidJobIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidJobIdException = _MatchServiceError textract "InvalidJobIdException"


-- | You aren't authorized to perform the action. Use the Amazon Resource Name (ARN) of an authorized user or IAM role to perform the operation.
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException = _MatchServiceError textract "AccessDeniedException"


-- | Amazon Textract isn't able to read the document. For more information on the document limits in Amazon Textract, see 'limits' .
--
--
_BadDocumentException :: AsError a => Getting (First ServiceError) a ServiceError
_BadDocumentException = _MatchServiceError textract "BadDocumentException"


-- | An input parameter violated a constraint. For example, in synchronous operations, an @InvalidParameterException@ exception occurs when neither of the @S3Object@ or @Bytes@ values are supplied in the @Document@ request parameter. Validate your parameter before calling the API operation again.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError textract "InvalidParameterException"


-- | The format of the input document isn't supported. Documents for synchronous operations can be in PNG or JPEG format. Documents for asynchronous operations can also be in PDF format.
--
--
_UnsupportedDocumentException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedDocumentException =
  _MatchServiceError textract "UnsupportedDocumentException"


-- | Amazon Textract is unable to access the S3 object that's specified in the request. for more information, <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Configure Access to Amazon S3> For troubleshooting information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/troubleshooting.html Troubleshooting Amazon S3>
--
--
_InvalidS3ObjectException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3ObjectException =
  _MatchServiceError textract "InvalidS3ObjectException"


-- | The number of requests exceeded your throughput limit. If you want to increase this limit, contact Amazon Textract.
--
--
_ProvisionedThroughputExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ProvisionedThroughputExceededException =
  _MatchServiceError textract "ProvisionedThroughputExceededException"


-- | Amazon Textract is temporarily unable to process the request. Try your call again.
--
--
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException = _MatchServiceError textract "ThrottlingException"


-- | Amazon Textract experienced a service issue. Try your call again.
--
--
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _MatchServiceError textract "InternalServerError"


-- | A @ClientRequestToken@ input parameter was reused with an operation, but at least one of the other input parameters is different from the previous call to the operation.
--
--
_IdempotentParameterMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_IdempotentParameterMismatchException =
  _MatchServiceError textract "IdempotentParameterMismatchException"


-- | Indicates you have exceeded the maximum number of active human in the loop workflows available
--
--
_HumanLoopQuotaExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_HumanLoopQuotaExceededException =
  _MatchServiceError textract "HumanLoopQuotaExceededException"


-- | The document can't be processed because it's too large. The maximum document size for synchronous operations 5 MB. The maximum document size for asynchronous operations is 500 MB for PDF files.
--
--
_DocumentTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_DocumentTooLargeException =
  _MatchServiceError textract "DocumentTooLargeException"


-- | An Amazon Textract service limit was exceeded. For example, if you start too many asynchronous jobs concurrently, calls to start operations (@StartDocumentTextDetection@ , for example) raise a LimitExceededException exception (HTTP status code: 400) until the number of concurrently running jobs is below the Amazon Textract service limit.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _MatchServiceError textract "LimitExceededException"

