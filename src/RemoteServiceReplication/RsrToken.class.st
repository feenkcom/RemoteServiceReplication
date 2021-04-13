Class {
	#name : 'RsrToken',
	#superclass : 'RsrTokenExchangeMessage',
	#instVars : [
		'bytes'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrToken class >> bytes: aByteArray [

	^self new
		bytes: aByteArray;
		yourself
]

{ #category : 'instance creation' }
RsrToken class >> newRandom [
	"Create a new Token with random bytes."

	| random bytes |
	random := HostRandom new.
	bytes := ByteArray new: 16.
	bytes unsigned32At: 1 put: random integer.
	bytes unsigned32At: 5 put: random integer.
	bytes unsigned32At: 9 put: random integer.
	bytes unsigned32At: 13 put: random integer.
	^self bytes: bytes
]

{ #category : 'comparing' }
RsrToken >> = aToken [

	^self class == aToken class and: [self bytes = aToken bytes]
]

{ #category : 'accessing' }
RsrToken >> bytes [

	^bytes
]

{ #category : 'accessing' }
RsrToken >> bytes: aByteArray [

	bytes := aByteArray
]

{ #category : 'hashing' }
RsrToken >> hash [

	^self bytes hash
]
