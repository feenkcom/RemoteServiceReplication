Class {
	#name : 'RsrServiceEphemeron',
	#superclass : 'RsrObject',
	#instVars : [
		'service',
		'action'
	],
	#category : 'RemoteServiceReplication-GemStone'
}

{ #category : 'instance creation' }
RsrServiceEphemeron class >> service: aService
action: aBlock [

	^self new
		service: aService;
		action: aBlock;
		beEphemeron: true;
		yourself
]

{ #category : 'accessing' }
RsrServiceEphemeron >> action [

	^action
]

{ #category : 'accessing' }
RsrServiceEphemeron >> action: aBlock [

	action := aBlock
]

{ #category : 'mourning' }
RsrServiceEphemeron >> mourn [

	action value.
	service := action := nil
]

{ #category : 'accessing' }
RsrServiceEphemeron >> service [

	^service
]

{ #category : 'accessing' }
RsrServiceEphemeron >> service: aService [

	service := aService
]
