Class {
	#name : 'RsrFalseSpecies',
	#superclass : 'RsrBooleanSpecies',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'coding' }
RsrFalseSpecies class >> decodeReference: aStream
using: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	^false
]

{ #category : 'accessing' }
RsrFalseSpecies class >> speciesIdentifier [

	^8
]
