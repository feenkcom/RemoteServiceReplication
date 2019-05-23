private-encoding
encodeObject: anObject
onto: aStream

	(self isService: anObject)
		ifTrue: [^self encodeService: anObject on: aStream].
	self error: 'Unable to encode: ', anObject printString