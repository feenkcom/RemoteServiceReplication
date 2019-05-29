private-encoding
encodeRetainObject: aRetainObject

	^ByteArray
		streamContents:
			[:stream |
			self
				encodeControlWord: self retainObjectIdentifier
				onto: stream.
			self
				encodeObject: aRetainObject object
				onto: stream]