Extension { #name : #RsrCharacterArraySpecies }

{ #category : #'*RemoteServiceReplication-Compatibility-Pharo' }
RsrCharacterArraySpecies class >> fromBytes: aByteArray [

	^aByteArray utf8Decoded
]

{ #category : #'*RemoteServiceReplication-Compatibility-Pharo' }
RsrCharacterArraySpecies class >> toBytes: aCharacterArray [

	^aCharacterArray utf8Encoded
]
