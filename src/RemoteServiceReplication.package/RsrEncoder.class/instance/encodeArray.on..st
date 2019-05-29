private-encoding
encodeArray: anArray
on: aStream

	self
		encodeControlWord: self arrayType
		onto: aStream.
	self
		encodeControlWord: anArray rsrId
		onto: aStream.
	self
		encodeControlWord: anArray size
		onto: aStream.
	anArray
		do:
			[:each |
			self
				encodeReferenceOf: each
				onto: aStream]