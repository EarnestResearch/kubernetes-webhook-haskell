let k8s =
      https://raw.githubusercontent.com/EarnestResearch/dhall-packages/master/kubernetes/k8s/1.14.dhall

let cert-manager =
      https://raw.githubusercontent.com/EarnestResearch/dhall-packages/master/kubernetes/cert-manager/package.dhall

let certsPath = "/certs"

let Config = ./Config.dhall

let labels = \(config : Config) -> toMap { app = config.name }

let deployment =
          \(config : Config)
      ->  k8s.Deployment::{
          , metadata = k8s.ObjectMeta::{ name = config.name }
          , spec =
              Some
                k8s.DeploymentSpec::{
                , selector = k8s.LabelSelector::{ matchLabels = labels config }
                , template =
                    k8s.PodTemplateSpec::{
                    , metadata =
                        k8s.ObjectMeta::{
                        , name = config.name
                        , labels = labels config
                        }
                    , spec =
                        Some
                          k8s.PodSpec::{
                          , containers =
                              [ k8s.Container::{
                                , name = config.name
                                , image = Some config.imageName
                                , ports =
                                    [ k8s.ContainerPort::{
                                      , containerPort = config.port
                                      }
                                    ]
                                , env =
                                    [ k8s.EnvVar::{
                                      , name = "CERTIFICATE_FILE"
                                      , value = Some "${certsPath}/tls.crt"
                                      }
                                    , k8s.EnvVar::{
                                      , name = "KEY_FILE"
                                      , value = Some "${certsPath}/tls.key"
                                      }
                                    ]
                                , volumeMounts =
                                    [ k8s.VolumeMount::{
                                      , name = "certs"
                                      , mountPath = certsPath
                                      , readOnly = Some True
                                      }
                                    ]
                                }
                              ]
                          , volumes =
                              [ k8s.Volume::{
                                , name = "certs"
                                , secret =
                                    Some
                                      k8s.SecretVolumeSource::{
                                      , secretName = Some config.name
                                      }
                                }
                              ]
                          }
                    }
                }
          }

let service =
          \(config : Config)
      ->  k8s.Service::{
          , metadata = k8s.ObjectMeta::{ name = config.name }
          , spec =
              Some
                k8s.ServiceSpec::{
                , selector = labels config
                , ports =
                    [ k8s.ServicePort::{
                      , targetPort = Some (k8s.IntOrString.Int config.port)
                      , port = 443
                      }
                    ]
                }
          }

let issuer =
          \(config : Config)
      ->  cert-manager.Issuer::{
          , metadata = k8s.ObjectMeta::{ name = config.name }
          , spec =
              cert-manager.IssuerSpec.SelfSigned
                cert-manager.SelfSignedIssuerSpec::{=}
          }

let certificate =
          \(config : Config)
      ->  cert-manager.Certificate::{
          , metadata = k8s.ObjectMeta::{ name = config.name }
          , spec =
              cert-manager.CertificateSpec::{
              , secretName = config.name
              , issuerRef = { name = config.name, kind = (issuer config).kind }
              , commonName = Some "${config.name}.${config.namespace}.svc"
              , dnsNames =
                  Some
                    [ config.name
                    , "${config.name}.${config.namespace}"
                    , "${config.name}.${config.namespace}.svc"
                    , "${config.name}.${config.namespace}.svc.cluster.local"
                    , "${config.name}:443"
                    , "${config.name}.${config.namespace}:443"
                    , "${config.name}.${config.namespace}.svc:443"
                    , "${config.name}.${config.namespace}.svc.cluster.local:443"
                    , "localhost:8080"
                    ]
              , usages = Some [ "any" ]
              , isCA = Some False
              }
          }

let mutatingWebhookConfiguration =
          \(config : Config)
      ->  k8s.MutatingWebhookConfiguration::{
          , metadata =
              k8s.ObjectMeta::{
              , name = config.name
              , labels = labels config
              , annotations =
                  toMap
                    { `cert-manager.io/inject-ca-from` =
                        "${config.namespace}/${config.name}"
                    }
              }
          , webhooks =
              [ k8s.Webhook::{
                , name = "${config.name}.${config.namespace}.svc"
                , clientConfig =
                    k8s.WebhookClientConfig::{
                    , service =
                        Some
                          { name = config.name
                          , namespace = config.namespace
                          , path = Some config.path
                          }
                    }
                , failurePolicy = config.failurePolicy
                , admissionReviewVersions = [ "v1beta1" ]
                , rules = config.rules
                , namespaceSelector = config.namespaceSelector
                }
              ]
          }

let union =
      < Deployment : k8s.Deployment.Type
      | Service : k8s.Service.Type
      | MWH : k8s.MutatingWebhookConfiguration.Type
      | Certificate : cert-manager.Certificate.Type
      | ClusterIssuer : cert-manager.ClusterIssuer.Type
      >

in      \(config : Config)
    ->  { apiVersion = "v1"
        , kind = "List"
        , items =
            [ union.Deployment (deployment config)
            , union.Service (service config)
            , union.MWH (mutatingWebhookConfiguration config)
            , union.Certificate (certificate config)
            , union.ClusterIssuer (issuer config)
            ]
        }
