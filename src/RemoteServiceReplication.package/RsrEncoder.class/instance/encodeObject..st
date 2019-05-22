encoding
encodeObject: anObject

	^ByteArray
		streamContents:
			[:stream |
			self
				encodeObject: anObject
				on: stream]