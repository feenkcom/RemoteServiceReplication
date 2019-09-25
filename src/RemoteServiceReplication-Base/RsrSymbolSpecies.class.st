Class {
	#name : #RsrSymbolSpecies,
	#superclass : #RsrCharacterArraySpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrSymbolSpecies class >> decodeReference: aStream for: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	^ (super decodeReference: aStream for: aDecoder) asSymbol
]

{ #category : #accessing }
RsrSymbolSpecies class >> speciesIdentifier [

	^1
]
