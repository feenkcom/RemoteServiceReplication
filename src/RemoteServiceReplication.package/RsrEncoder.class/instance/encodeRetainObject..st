private-encoding
encodeRetainObject: aRetainObject

	^ByteArray
		streamContents:
			[:stream |
			self
				writeInteger: self retainObjectIdentifier
				onto: stream.
			self
				encodeObject: aRetainObject object
				on: stream]