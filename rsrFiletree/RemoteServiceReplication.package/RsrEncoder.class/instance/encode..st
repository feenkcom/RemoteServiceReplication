encoding
encode: anObject

	| encoding |
	encoding := ByteArray
		streamContents:
			[:stream |
			self
				encode: anObject
				on: stream].
	^RsrEncodedObject
		object: anObject
		encoding: encoding