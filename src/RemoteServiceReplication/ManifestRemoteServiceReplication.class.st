"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestRemoteServiceReplication,
	#superclass : #PackageManifest,
	#category : #'RemoteServiceReplication-Manifest'
}

{ #category : #'code-critics' }
ManifestRemoteServiceReplication class >> ruleRBBadMessageRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#RsrDecoder #decodeService:ofLength: #false)) #'2019-05-20T13:17:59.370357-07:00') )
]

{ #category : #'code-critics' }
ManifestRemoteServiceReplication class >> ruleRBCollectionMessagesToExternalObjectRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#RsrRetainObject #executeFor: #false)) #'2019-05-21T09:06:20.697349-07:00') )
]

{ #category : #'code-critics' }
ManifestRemoteServiceReplication class >> ruleRBSendsDifferentSuperRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'RsrObject class' #rsrId:rsrRemoteInterface: #true)) #'2019-05-20T12:06:31.629509-07:00') )
]

{ #category : #'code-critics' }
ManifestRemoteServiceReplication class >> ruleRBSentNotImplementedRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#RsrRetainObject #executeFor: #false)) #'2019-05-21T09:06:23.559069-07:00') )
]

{ #category : #'code-critics' }
ManifestRemoteServiceReplication class >> ruleRBSuperSendsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'RsrObject class' #rsrId:rsrRemoteInterface: #true)) #'2019-05-20T12:06:22.813904-07:00') )
]
