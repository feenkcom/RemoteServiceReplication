Extension { #name : 'RsrCharacterArraySpecies' }

{ #category : '*remoteservicereplication-gemstone' }
RsrCharacterArraySpecies class >> fromBytes: aByteArray [

	^aByteArray decodeFromUTF8ToString
]

{ #category : '*remoteservicereplication-gemstone' }
RsrCharacterArraySpecies class >> toBytes: aCharacterArray [

	^aCharacterArray encodeAsUTF8
]
