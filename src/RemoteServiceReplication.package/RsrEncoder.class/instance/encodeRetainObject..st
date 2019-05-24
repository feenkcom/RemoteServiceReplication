private-encoding
encodeRetainObject: aRetainObject

	^ByteArray
		streamContents:
			[:stream |
			self
				writeControlWord: self retainObjectIdentifier
				onto: stream.
			self
				encodeObject: aRetainObject object
				onto: stream]