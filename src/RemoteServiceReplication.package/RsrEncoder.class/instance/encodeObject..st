encoding
encodeObject: anObject

	^ByteArray
		streamContents:
			[:stream |
			self
				encodeObject: anObject
				onto: stream]