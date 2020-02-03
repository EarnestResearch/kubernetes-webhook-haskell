{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Kubernetes.Webhook.Types where

import qualified Data.Aeson as A
import Data.Aeson ((.:))
import Data.Binary.Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Char as C
import Data.HashMap.Strict
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics

-- |
-- This is the type of the request that arrives for the admission webhook
-- see https://godoc.org/k8s.io/api/admission/v1beta1#AdmissionReview
data AdmissionReviewRequest
  = AdmissionReviewRequest
      { apiVersion :: Text,
        kind :: Text,
        request :: AdmissionRequest
      }
  deriving (Generic, Show, A.FromJSON, A.ToJSON)

-- |
-- This is the type of the response returned to the admission webhook
-- see https://godoc.org/k8s.io/api/admission/v1beta1#AdmissionReview
data AdmissionReviewResponse
  = AdmissionReviewResponse
      { apiVersion :: Text,
        kind :: Text,
        response :: AdmissionResponse
      }
  deriving (Generic, Show, A.FromJSON, A.ToJSON)

admissionReviewResponse :: AdmissionResponse -> AdmissionReviewResponse
admissionReviewResponse resp =
  AdmissionReviewResponse
    { apiVersion = "admission.k8s.io/v1beta1",
      kind = "AdmissionReview",
      response = resp
    }

data AdmissionRequest
  = AdmissionRequest
      { -- |
        -- UID is an identifier for the individual request response. It allows us to distinguish instances of requests which are
        -- otherwise identical (parallel requests, requests when earlier requests did not modify etc)
        -- The UID is meant to track the round trip (request response) between the KAS and the WebHook, not the user request.
        -- It is suitable for correlating log entries between the webhook and apiserver, for either auditing or debugging.
        uid :: UID,
        -- |
        -- Kind is the fully-qualified type of object being submitted (for example, v1.Pod or autoscaling.v1.Scale)
        kind :: GroupVersionKind,
        -- |
        -- Resource is the fully-qualified resource being requested (for example, v1.pods)
        resource :: GroupVersionResource,
        -- |
        -- SubResource is the subresource being requested, if any (for example, "status" or "scale")
        subResource :: Maybe Text,
        -- |
        -- RequestKind is the fully-qualified type of the original API request (for example, v1.Pod or autoscaling.v1.Scale).
        -- If this is specified and differs from the value in "kind", an equivalent match and conversion was performed.
        --
        -- For example, if deployments can be modified via apps v1 and apps v1beta1, and a webhook registered a rule of
        -- `apiGroups:["apps"], apiVersions:["v1"], resources: ["deployments"]` and `matchPolicy: Equivalent`,
        -- an API request to apps v1beta1 deployments would be converted and sent to the webhook
        -- with `kind: {group:"apps", version:"v1", kind:"Deployment"}` (matching the rule the webhook registered for),
        -- and `requestKind: {group:"apps", version:"v1beta1", kind:"Deployment"}` (indicating the kind of the original API request).
        --
        -- See documentation for the "matchPolicy" field in the webhook configuration type for more details.
        requestKind :: Maybe GroupVersionKind,
        -- |
        -- RequestResource is the fully-qualified resource of the original API request (for example, v1.pods).
        -- If this is specified and differs from the value in "resource", an equivalent match and conversion was performed.
        --
        -- For example, if deployments can be modified via apps v1 and apps v1beta1, and a webhook registered a rule of
        -- `apiGroups:["apps"], apiVersions:["v1"], resources: ["deployments"]` and `matchPolicy: Equivalent`,
        -- an API request to apps v1beta1 deployments would be converted and sent to the webhook
        -- with `resource: {group:"apps", version:"v1", resource:"deployments"}` (matching the resource the webhook registered for),
        -- and `requestResource: {group:"apps", version:"v1beta1", resource:"deployments"}` (indicating the resource of the original API request).
        --
        -- See documentation for the "matchPolicy" field in the webhook configuration type.
        requestResource :: Maybe GroupVersionResource,
        -- |
        -- RequestSubResource is the name of the subresource of the original API request, if any (for example, "status" or "scale")
        -- If this is specified and differs from the value in "subResource", an equivalent match and conversion was performed.
        -- See documentation for the "matchPolicy" field in the webhook configuration type.
        requestSubResource :: Maybe Text,
        -- |
        -- Name is the name of the object as presented in the request.  On a CREATE operation, the client may omit name and
        -- rely on the server to generate the name.  If that is the case, this field will contain an empty string.
        name :: Maybe Text,
        -- |
        -- Namespace is the namespace associated with the request (if any).
        namespace :: Maybe Text,
        -- |
        -- Operation is the operation being performed. This may be different than the operation
        -- requested. e.g. a patch can result in either a CREATE or UPDATE Operation.
        operation :: Operation,
        -- |
        -- UserInfo is information about the requesting user
        userInfo :: UserInfo,
        -- |
        -- Object is the object from the incoming request.
        object :: Maybe A.Value,
        -- |
        -- OldObject is the existing object. Only populated for DELETE and UPDATE requests.
        oldObject :: Maybe A.Value,
        -- |
        -- DryRun indicates that modifications will definitely not be persisted for this request.
        -- Defaults to false.
        dryRun :: Maybe Bool,
        -- |
        -- Options is the operation option structure of the operation being performed.
        -- e.g. `meta.k8s.io/v1.DeleteOptions` or `meta.k8s.io/v1.CreateOptions`. This may be
        -- different than the options the caller provided. e.g. for a patch request the performed
        -- Operation might be a CREATE, in which case the Options will a
        -- `meta.k8s.io/v1.CreateOptions` even though the caller provided `meta.k8s.io/v1.PatchOptions`.
        options :: Maybe A.Value
      }
  deriving (Generic, Show, A.FromJSON, A.ToJSON)

-- |
-- AdmissionResponse describes an admission response.
-- see: https://godoc.org/k8s.io/api/admission/v1beta1#AdmissionResponse
data AdmissionResponse
  = AdmissionResponse
      { -- | UID is an identifier for the individual request/response.
        uid :: UID,
        -- | Allowed indicates whether or not the admission request was permitted
        allowed :: Bool,
        -- |
        -- Result contains extra details into why an admission request was denied.
        -- This field IS NOT consulted in any way if "Allowed" is "true".
        status :: Maybe Status,
        -- |
        -- The patch body. Currently we only support "JSONPatch" which implements RFC 6902.
        patch :: Maybe Patch,
        -- |
        -- The type of Patch. Currently we only allow "JSONPatch".
        patchType :: Maybe PatchType,
        -- |
        -- AuditAnnotations is an unstructured key value map set by remote admission controller (e.g. error=image-blacklisted).
        -- MutatingAdmissionWebhook and ValidatingAdmissionWebhook admission controller will prefix the keys with
        -- admission webhook name (e.g. imagepolicy.example.com/error=image-blacklisted). AuditAnnotations will be provided by
        -- the admission webhook to add additional context to the audit log for this request.
        auditAnnotations :: Maybe (HashMap Text [Text])
      }
  deriving (Generic, Show, A.ToJSON, A.FromJSON)

-- |
-- UID is a type that holds unique ID values, including UUIDs. Because we don't ONLY use UUIDs, this is an alias to string
-- Being a type captures intent and helps make sure that UIDs and names do not get conflated.
newtype UID = UID Text deriving (Generic, Show, A.ToJSON, A.FromJSON)

-- |
-- Operation is the type of resource operation being checked for admission control
data Operation = CREATE | UPDATE | DELETE | CONNECT deriving (Generic, Show, A.ToJSON, A.FromJSON)

-- |
-- UserInfo holds the information about the user needed to implement the user.Info interface.
data UserInfo
  = UserInfo
      { -- | The name that uniquely identifies this user among all active users.
        username :: Text,
        -- | A unique value that identifies this user across time. If this user is
        -- deleted and another user by the same name is added, they will have
        -- different UIDs.
        uid :: Text,
        -- |
        -- The names of groups this user is a part of.
        groups :: Maybe [Text],
        -- |
        -- Any additional information provided by the authenticator.
        extra :: Maybe (HashMap Text [Text])
      }
  deriving (Generic, Show, A.ToJSON, A.FromJSON)

-- |
-- GroupVersionKind unambiguously identifies a kind.
data GroupVersionKind
  = GroupVersionKind
      { group :: Text,
        version :: Text,
        kind :: Text
      }
  deriving (Generic, Show, A.ToJSON, A.FromJSON)

-- |
-- GroupVersionResource unambiguously identifies a resource.
data GroupVersionResource
  = GroupVersionResource
      { group :: Text,
        version :: Text,
        resource :: Text
      }
  deriving (Generic, Show, A.ToJSON, A.FromJSON)

-- |
-- RawExtension is used to hold extensions in external versions.
data RawExtension
  = RawExtension
      { raw :: Text,
        object :: A.Value
      }
  deriving (Generic, Show, A.ToJSON, A.FromJSON)

-- |
-- Status is a return value for calls that don't return other objects.
data Status
  = Status
      { typeMeta :: TypeMeta,
        -- |
        -- Standard list metadata.
        -- More info: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#types-kinds
        listMeta :: Maybe ListMeta,
        -- |
        -- Status of the operation.
        -- One of: "Success" or "Failure".
        -- More info: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#spec-and-status
        status :: Maybe StatusStatus,
        -- |
        -- A human-readable description of the status of this operation.
        message :: Maybe Text,
        -- |
        -- A machine-readable description of why this operation is in the
        -- "Failure" status. If this value is empty there
        -- is no information available. A Reason clarifies an HTTP status
        -- code but does not override it.
        reason :: Maybe StatusReason,
        -- |
        -- Extended data associated with the reason.  Each reason may define its
        -- own extended details. This field is optional and the data returned
        -- is not guaranteed to conform to any schema except that defined by
        -- the reason type.
        details :: Maybe StatusDetails,
        -- |
        -- Suggested HTTP return code for this status, 0 if not set.
        code :: Maybe Integer
      }
  deriving (Generic, Show, A.ToJSON, A.FromJSON)

data ListMeta
  = ListMeta
      { -- |
        -- selfLink is a URL representing this object.
        -- Populated by the system.
        -- Read-only.
        --
        -- DEPRECATED
        -- Kubernetes will stop propagating this field in 1.20 release and the field is planned
        -- to be removed in 1.21 release.
        selfLink :: Maybe Text,
        -- |
        -- String that identifies the server's internal version of this object that
        -- can be used by clients to determine when objects have changed.
        -- Value must be treated as opaque by clients and passed unmodified back to the server.
        -- Populated by the system.
        -- Read-only.
        -- More info: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#concurrency-control-and-consistency
        resourceVersion :: Maybe Text,
        -- |
        -- continue may be set if the user set a limit on the number of items returned, and indicates that
        -- the server has more data available. The value is opaque and may be used to issue another request
        -- to the endpoint that served this list to retrieve the next set of available objects. Continuing a
        -- consistent list may not be possible if the server configuration has changed or more than a few
        -- minutes have passed. The resourceVersion field returned when using this continue value will be
        -- identical to the value in the first response, unless you have received this token from an error
        -- message.
        continue :: Text,
        -- |
        -- remainingItemCount is the number of subsequent items in the list which are not included in this
        -- list response. If the list request contained label or field selectors, then the number of
        -- remaining items is unknown and the field will be left unset and omitted during serialization.
        -- If the list is complete (either because it is not chunking or because this is the last chunk),
        -- then there are no more remaining items and this field will be left unset and omitted during
        -- serialization.
        -- Servers older than v1.15 do not set this field.
        -- The intended use of the remainingItemCount is *estimating* the size of a collection. Clients
        -- should not rely on the remainingItemCount to be set or to be exact.
        remainingItemCount :: Maybe Integer
      }
  deriving (Generic, Show, A.ToJSON, A.FromJSON)

data StatusStatus = Success | Failure deriving (Generic, Show, A.ToJSON, A.FromJSON)

-- |
-- StatusReason is an enumeration of possible failure causes.
-- Each StatusReason must map to a single HTTP status code, but multiple reasons may map to the same HTTP status code.
-- https://godoc.org/k8s.io/apimachinery/pkg/apis/meta/v1#StatusReason
data StatusReason
  = Unknown
  | Unauthorized
  | Forbidden
  | NotFound
  | AlreadyExists
  | Conflict
  | Gone
  | Invalid
  | ServerTimeout
  | Timeout
  | TooManyRequests
  | BadRequest
  | MethodNotAllowed
  | NotAcceptable
  | RequestEntityTooLarge
  | UnsupportedMediaType
  | InternalError
  | Expired
  | ServiceUnavailable
  deriving (Generic, Show, A.ToJSON, A.FromJSON)

-- |
-- StatusDetails is a set of additional properties that MAY be set by the server to provide additional information
-- about a response. The Reason field of a Status object defines what attributes will be set. Clients must ignore
-- fields that do not match the defined type of each attribute, and should assume that any attribute may be empty,
-- invalid, or under defined.
data StatusDetails
  = StatusDetails
      { -- |
        -- The name attribute of the resource associated with the status StatusReason
        -- (when there is a single name which can be described).
        name :: Maybe Text,
        -- |
        -- The group attribute of the resource associated with the status StatusReason.
        group :: Maybe Text,
        -- |
        -- The kind attribute of the resource associated with the status StatusReason.
        -- On some operations may differ from the requested resource Kind.
        -- More info: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#types-kinds
        kind :: Maybe Text,
        -- |
        -- UID of the resource.
        -- (when there is a single resource which can be described).
        -- More info: http://kubernetes.io/docs/user-guide/identifiers#uids
        uid :: Maybe UID,
        -- |
        -- The Causes array includes more details associated with the StatusReason
        -- failure. Not all StatusReasons may provide detailed causes.
        causes :: Maybe [StatusCause],
        -- |
        -- If specified, the time in seconds before the operation should be retried. Some errors may indicate
        -- the client must take an alternate action - for those errors this field may indicate how long to wait
        -- before taking the alternate action.
        retryAfterSeconds :: Maybe Integer
      }
  deriving (Generic, Show, A.ToJSON, A.FromJSON)

data StatusCause
  = StatusCause
      { -- |
        -- A machine-readable description of the cause of the error. If this value is
        -- empty there is no information available.
        reason :: Maybe CauseType,
        -- |
        -- human-readable description of the cause of the error.  This field may be
        -- presented as-is to a reader.
        message :: Maybe Text,
        -- |
        -- The field of the resource that has caused this error, as named by its JSON
        -- serialization. May include dot and postfix notation for nested attributes.
        -- Arrays are zero-indexed.  Fields may appear more than once in an array of
        -- causes due to fields having multiple errors.
        -- Optional.
        --
        -- Examples:
        -- "name" - the field "name" on the current resource
        -- "items[0].name" - the field "name" on the first array entry in "items"
        field :: Maybe Text
      }
  deriving (Generic, Show, A.ToJSON, A.FromJSON)

-- |
-- CauseType is a machine readable value providing more detail about what occurred in a status response. An operation may have multiple causes for a status (whether Failure or Success).
data CauseType
  = FieldValueNotFound
  | FieldValueRequired
  | FieldValueDuplicate
  | FieldValueInvalid
  | FieldValueNotSupported
  | UnexpectedServerResponse
  | FieldManagerConflict
  deriving (Generic, Show, A.ToJSON, A.FromJSON)

data TypeMeta
  = TypeMeta
      { -- |
        -- Kind is a string value representing the REST resource this object represents.
        -- Servers may infer this from the endpoint the client submits requests to.
        -- Cannot be updated.
        -- In CamelCase.
        -- More info: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#types-kinds
        kind :: Maybe Text,
        -- |
        -- APIVersion defines the versioned schema of this representation of an object.
        -- Servers should convert recognized schemas to the latest internal value, and
        -- may reject unrecognized values.
        -- More info: https://git.k8s.io/community/contributors/devel/sig-architecture/api-conventions.md#resources
        apiVersion :: Maybe Text
      }
  deriving (Generic, Show, A.ToJSON, A.FromJSON)

data PatchType = JSONPatch deriving (Show)

instance A.ToJSON PatchType where
  toJSON JSONPatch = A.String "JSONPatch"

instance A.FromJSON PatchType where
  parseJSON (A.String "JSONPatch") = pure JSONPatch
  parseJSON v = fail $ "Expected \"JSONPatch\". Got " <> show v

data PatchOperation
  = PatchOperation
      { op :: PatchOp,
        path :: Text,
        from :: Maybe Text,
        value :: Maybe A.Value
      }
  deriving (Generic, Show, A.FromJSON, A.ToJSON)

-- | Patch type as per RFC-6902
-- See http://jsonpatch.com for documentation
newtype Patch = Patch [PatchOperation] deriving (Generic, Show)

-- | The Patch needs to be base64-encoded
instance-- to keep the nice types we customize the toJSON encoding
  A.ToJSON Patch where
  toJSON =
    A.String
      . TE.decodeUtf8
      . Base64.encode
      . BSL.toStrict
      . toLazyByteString
      . A.fromEncoding
      . A.genericToEncoding A.defaultOptions

instance A.FromJSON Patch where
  parseJSON (A.String p) =
    let decoded = (Base64.decode . TE.encodeUtf8) p >>= A.eitherDecodeStrict
     in case decoded of
          Left e -> fail e
          Right v -> pure v
  parseJSON v = fail $ "Invalid type. Expected String, got: " <> show v

data PatchOp = Add | Copy | Move | Remove | Replace | Test deriving (Generic, Show)

instance A.ToJSON PatchOp where
  toJSON = A.genericToJSON opts
    where
      opts = A.defaultOptions {A.constructorTagModifier = lowerFirst}

instance A.FromJSON PatchOp where
  parseJSON = A.genericParseJSON opts
    where
      opts = A.defaultOptions {A.constructorTagModifier = lowerFirst}

lowerFirst :: String -> String
lowerFirst t = fmap C.toLower (take 1 t) <> drop 1 t
