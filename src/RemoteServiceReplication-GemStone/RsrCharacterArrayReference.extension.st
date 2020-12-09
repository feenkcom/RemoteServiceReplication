Extension { #name : 'RsrCharacterArrayReference' }

{ #category : '*remoteservicereplication-gemstone' }
RsrCharacterArrayReference class >> convertToBytes: aCharacterArray [

	^aCharacterArray encodeAsUTF8
]

{ #category : '*remoteservicereplication-gemstone' }
RsrCharacterArrayReference >> convertBytes: aByteArray [

	^aByteArray decodeFromUTF8ToString
]
