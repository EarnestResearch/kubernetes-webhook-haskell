let k8s =
      https://raw.githubusercontent.com/EarnestResearch/dhall-packages/master/kubernetes/k8s/1.14.dhall sha256:7839bf40f940757e4d71d3c1b84d878f6a4873c3b2706ae4be307b5991acdcac

in  { name : Text
    , namespace : Text
    , imageName : Text
    , namespaceSelector : Optional k8s.LabelSelector.Type
    , port : Natural
    , path : Text
    , failurePolicy : Optional Text
    , rules : List k8s.RuleWithOperations.Type
    }
