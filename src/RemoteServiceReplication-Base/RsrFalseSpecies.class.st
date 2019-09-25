Class {
	#name : #RsrFalseSpecies,
	#superclass : #RsrBooleanSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrFalseSpecies class >> decodeWith: aDecoder
from: aStream [
	"Decode the provided bytes into the default native class for this species"

	^false
]

{ #category : #accessing }
RsrFalseSpecies class >> speciesIdentifier [

	^8
]
