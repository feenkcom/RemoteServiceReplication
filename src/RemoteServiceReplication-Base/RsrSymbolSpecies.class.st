Class {
	#name : #RsrSymbolSpecies,
	#superclass : #RsrCharacterArraySpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrSymbolSpecies class >> decodeWith: aDecoder
from: aStream [
	"Decode the provided bytes into the default native class for this species"

	^(super decodeWith: aDecoder from: aStream) asSymbol
]

{ #category : #accessing }
RsrSymbolSpecies class >> speciesIdentifier [

	^1
]
