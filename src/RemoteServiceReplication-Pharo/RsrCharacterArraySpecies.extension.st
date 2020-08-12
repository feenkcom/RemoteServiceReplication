Extension { #name : 'RsrCharacterArraySpecies' }

{ #category : '*RemoteServiceReplication-Pharo' }
RsrCharacterArraySpecies class >> fromBytes: aByteArray [

	^aByteArray utf8Decoded
]

{ #category : '*RemoteServiceReplication-Pharo' }
RsrCharacterArraySpecies class >> toBytes: aCharacterArray [

	^aCharacterArray utf8Encoded
]
