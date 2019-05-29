private-encoding
encodeReferenceOf: anObject
onto: aStream

	(self isRsrObject: anObject)
		ifTrue: [^self encodeControlWord: anObject rsrId onto: aStream].
	(self isImmediate: anObject)
		ifTrue: [^self encodeImmediate: anObject onto: aStream].
	self error: 'Unsupported type'