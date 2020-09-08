Extension { #name : 'RsrCharacterArrayReference' }

{ #category : '*remoteservicereplication-gemstone' }
RsrCharacterArrayReference >> convertBytes: aByteArray [

	^aByteArray decodeFromUTF8ToString
]

{ #category : '*remoteservicereplication-gemstone' }
RsrCharacterArrayReference >> convertToBytes: aCharacterArray [

	^aCharacterArray encodeAsUTF8
]
