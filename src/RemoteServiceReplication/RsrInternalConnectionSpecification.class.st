Class {
	#name : 'RsrInternalConnectionSpecification',
	#superclass : 'RsrConnectionSpecification',
	#instVars : [
		'connectionA',
		'connectionB'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'asserting' }
RsrInternalConnectionSpecification >> assertOpen [
	"Assert that connectionA and connectionB are open.
	Signal RsrConnectionFailed if they are not."

	(connectionA isOpen and: [connectionB isOpen])
		ifFalse: [RsrConnectionFailed signal]
]

{ #category : 'connecting' }
RsrInternalConnectionSpecification >> connect [
	"Establish an internal Connection pair."

	self subclassResponsibility
]

{ #category : 'accessing' }
RsrInternalConnectionSpecification >> connectionA [

	^connectionA
]

{ #category : 'accessing' }
RsrInternalConnectionSpecification >> connectionB [

	^connectionB
]
