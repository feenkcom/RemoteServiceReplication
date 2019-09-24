Class {
	#name : #RsrSymbolSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrSymbolSpecies class >> encode: aSymbol
with: anEncoder
onto: aStream [

	^anEncoder
		encodeSymbol: aSymbol
		onto: aStream
]
