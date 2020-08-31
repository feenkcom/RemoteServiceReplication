Class {
	#name : 'RsrServiceReference',
	#superclass : 'RsrReference',
	#instVars : [
		'sid'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrServiceReference class >> from: aService [

	^self sid: aService _id
]

{ #category : 'instance creation' }
RsrServiceReference class >> sid: aServiceID [

	^self new
		sid: aServiceID;
		yourself
]

{ #category : 'encoding/decoding' }
RsrServiceReference >> encode: aStream
using: anEncoder [

	anEncoder
		encodeControlWord: self sid
		onto: aStream
]

{ #category : 'resolving' }
RsrServiceReference >> resolve: aRegistry [

	^aRegistry
		serviceAt: self sid
		ifAbsent: [^RsrUnknownOID signal: self sid printString]
]

{ #category : 'accessing' }
RsrServiceReference >> sid [

	^sid
]

{ #category : 'accessing' }
RsrServiceReference >> sid: aServiceID [

	sid := aServiceID
]
