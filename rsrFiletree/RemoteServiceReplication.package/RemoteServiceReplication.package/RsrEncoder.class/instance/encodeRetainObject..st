encoding
encodeRetainObject: aRetainObject

	^ByteArray
		streamContents:
			[:stream |
			stream nextPutAll: (self retainObjectIdentifier asByteArrayOfSize: 8).
			self
				encodeObject: aRetainObject object
				on: stream]