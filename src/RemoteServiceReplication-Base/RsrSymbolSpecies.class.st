Class {
	#name : #RsrSymbolSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrSymbolSpecies class >> encodeReference: aSymbol
with: anEncoder
onto: aStream [

	^anEncoder
		encodeSymbol: aSymbol
		onto: aStream
]

{ #category : #accessing }
RsrSymbolSpecies class >> speciesIdentifier [

	^1
]
