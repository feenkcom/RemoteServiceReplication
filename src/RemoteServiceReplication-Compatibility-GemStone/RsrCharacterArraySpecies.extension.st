Extension { #name : 'RsrCharacterArraySpecies' }

{ #category : '*remoteservicereplication-compatibility-gemstone' }
RsrCharacterArraySpecies class >> fromBytes: aByteArray [

	^aByteArray decodeFromUTF8ToString
]

{ #category : '*remoteservicereplication-compatibility-gemstone' }
RsrCharacterArraySpecies class >> toBytes: aCharacterArray [

	^aCharacterArray encodeAsUTF8
]
