encoding
encode: anObject

	| encoding |
	encoding := ByteArray
		streamContents:
			[:stream |
			self
				encode: anObject
				on: stream].
	^RsrRetainObject
		object: anObject
		encoding: encoding