Class {
	#name : #RsrSymbolSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrSymbolSpecies class >> decodeWith: aDecoder
from: aStream [
	"Decode the provided bytes into the default native class for this species"

	^(RsrStringSpecies decodeWith: aDecoder from: aStream) asSymbol
]

{ #category : #coding }
RsrSymbolSpecies class >> encodeReference: aSymbol
with: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: self immediateOID
		onto: aStream.
	anEncoder
		encodeControlWord: self speciesIdentifier
		onto: aStream.
	anEncoder
		encodeStringBody: aSymbol
		onto: aStream
]

{ #category : #accessing }
RsrSymbolSpecies class >> speciesIdentifier [

	^1
]
