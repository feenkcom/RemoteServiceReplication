Class {
	#name : 'RsrRegistryEntry',
	#superclass : 'RsrObject',
	#instVars : [
		'ephemeron',
		'strongStorage'
	],
	#category : 'RemoteServiceReplication-GemStone'
}

{ #category : 'instance creation' }
RsrRegistryEntry class >> service: aService
onMourn: aBlock [

	| ephemeron |
	ephemeron := RsrServiceEphemeron
		service: aService
		action: aBlock.
	^self new
		ephemeron: ephemeron;
		yourself
]

{ #category : 'transitions' }
RsrRegistryEntry >> becomeStrong [

	strongStorage := self service
]

{ #category : 'transitions' }
RsrRegistryEntry >> becomeWeak [

	strongStorage := nil
]

{ #category : 'private-accessing' }
RsrRegistryEntry >> ephemeron: aServiceEphemeron [

	ephemeron := aServiceEphemeron
]

{ #category : 'accessing' }
RsrRegistryEntry >> service [

	^ephemeron service
]
