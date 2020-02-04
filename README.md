# kubernetes-webhook-haskell

This library lets you create [Kubernetes Admission Webhooks](https://kubernetes.io/docs/reference/access-authn-authz/extensible-admission-controllers/) in Haskell.

Using webhooks in Kubernetes requires some configuration, in the [dhall](./dhall) directory you can find some useful templates that you can reuse to deploy your webhooks. [cert-manager](https://cert-manager.io/docs/) is required to be installed in the cluster to use the templates.

Example webhook using Servant:
```hs
module Kubernetes.Example
    ( startApp,
    app,
    )
where

import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Text
import GHC.Generics
import qualified Kubernetes.Webhook as W
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Servant
import System.Environment

type API =
"mutate" :> ReqBody '[JSON] W.AdmissionReviewRequest :> Post '[JSON] W.AdmissionReviewResponse

data Toleration
= Toleration
    { effect :: Maybe TolerationEffect,
        key :: Maybe Text,
        operator :: Maybe TolerationOperator,
        tolerationSeconds :: Maybe Integer,
        value :: Maybe Text
    }
deriving (Generic, A.ToJSON)

data TolerationEffect = NoSchedule | PreferNoSchedule | NoExecute deriving (Generic, A.ToJSON)

data TolerationOperator = Exists | Equal deriving (Generic, A.ToJSON)

testToleration :: Toleration
testToleration =
Toleration
    { effect = Just NoSchedule,
    key = Just "dedicated",
    operator = Just Equal,
    tolerationSeconds = Nothing,
    value = Just "test"
    }

startApp :: IO ()
startApp = do
let tlsOpts = tlsSettings "/certs/tls.crt" "/certs/tls.key"
    warpOpts = setPort 8080 defaultSettings
runTLS tlsOpts warpOpts app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = mutate

mutate :: W.AdmissionReviewRequest -> Handler W.AdmissionReviewResponse
mutate req = pure $ W.mutatingWebhook req (\_ -> Right W.Allowed) addToleration

addToleration :: W.Patch
addToleration = 
W.Patch
    [ W.PatchOperation
        { op = W.Add,
        path = "/spec/tolerations/-",
        from = Nothing,
        value = Just $ A.toJSON testToleration
        }
    ]
```

