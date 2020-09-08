Extension { #name : 'RsrCharacterArrayReference' }

{ #category : '*RemoteServiceReplication-Pharo' }
RsrCharacterArrayReference >> convertBytes: aByteArray [

	^aByteArray utf8Decoded
]

{ #category : '*RemoteServiceReplication-Pharo' }
RsrCharacterArrayReference >> convertToBytes: aCharacterArray [

	^aCharacterArray utf8Encoded

]
