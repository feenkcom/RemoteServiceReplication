Class {
	#name : 'RsrServiceReference',
	#superclass : 'RsrReference',
	#instVars : [
		'sid'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrServiceReference class >> sid: aServiceID [

	^self new
		sid: aServiceID;
		yourself
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
