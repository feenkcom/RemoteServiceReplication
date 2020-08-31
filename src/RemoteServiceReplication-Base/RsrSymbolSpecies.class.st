Class {
	#name : 'RsrSymbolSpecies',
	#superclass : 'RsrCharacterArraySpecies',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'coding' }
RsrSymbolSpecies class >> decodeReference: aStream using: aDecoder [
	"Decode the provided bytes into the default native class for this species"

	^ (super decodeReference: aStream using: aDecoder) asSymbol
]

{ #category : 'accessing' }
RsrSymbolSpecies class >> speciesIdentifier [

	^1
]
