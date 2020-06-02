"
I represent a class of Objects that know offer Rsr Services.
"
Class {
	#name : 'RsrService',
	#superclass : 'RsrAbstractService',
	#instVars : [
		'_id',
		'_connection',
		'remoteSelf'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrService class >> _id: anId
connection: aConnection [

	^super new
		_id: anId connection: aConnection;
		yourself
]

{ #category : 'accessing' }
RsrService class >> clientClass [

	^RsrClassResolver classNamed: self clientClassName
]

{ #category : 'accessing' }
RsrService class >> clientClassName [

	self subclassResponsibility
]

{ #category : 'testing' }
RsrService class >> isClientClass [

	^self name == self clientClassName
]

{ #category : 'testing' }
RsrService class >> isServerClass [

	^self name == self serverClassName
]

{ #category : 'testing' }
RsrService class >> isTemplateClass [

	^self name == self templateClassName
]

{ #category : 'accessing' }
RsrService class >> serverClass [

	^RsrClassResolver classNamed: self serverClassName
]

{ #category : 'accessing' }
RsrService class >> serverClassName [

	self subclassResponsibility
]

{ #category : 'accessing' }
RsrService class >> templateClass [

	^RsrClassResolver classNamed: self templateClassName
]

{ #category : 'accessing' }
RsrService class >> templateClassName [

	self subclassResponsibility
]

{ #category : 'private-accessing' }
RsrService >> _connection [

	^_connection
]

{ #category : 'private-accessing' }
RsrService >> _id [

	^_id
]

{ #category : 'private-accessing' }
RsrService >> _id: anRsrId
connection: aConnection [

	_id := anRsrId.
	_connection := aConnection.
	remoteSelf := aConnection _forwarderClass on: self
]

{ #category : 'private-synchronization' }
RsrService >> _synchronize [
	"Return self to synchronize with the remote peer"

	^self
]

{ #category : 'public-testing' }
RsrService >> isClient [

	^self class isClientClass
]

{ #category : 'public-testing' }
RsrService >> isMirrored [

	^_connection ~~ nil
]

{ #category : 'public-testing' }
RsrService >> isNotMirrored [

	^self isMirrored not
]

{ #category : 'public-testing' }
RsrService >> isServer [

	^self class isServerClass
]

{ #category : 'public-accessing' }
RsrService >> reflectedVariableNames [

	^RsrServiceSpecies reflectedVariablesFor: self
]

{ #category : 'public-synchronization' }
RsrService >> synchronize [

	remoteSelf == nil
		ifFalse: [remoteSelf _synchronize]
]
