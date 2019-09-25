Extension { #name : #RsrCharacterArraySpecies }

{ #category : #'*RemoteServiceReplication-Compatibility-GemStone' }
RsrCharacterArraySpecies class >> toBytes: aCharacterArray [

	^aCharacterArray encodeAsUTF8
]

{ #category : #'*RemoteServiceReplication-Compatibility-GemStone' }
RsrCharacterArraySpecies class >> fromBytes: aByteArray [

	^aByteArray decodeFromUTF8ToString
]
