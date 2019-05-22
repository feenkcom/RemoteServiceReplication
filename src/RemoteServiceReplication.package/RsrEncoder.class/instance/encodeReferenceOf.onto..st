private-encoding
encodeReferenceOf: anObject
onto: aStream

	(self isRsrObject: anObject)
		ifTrue: [^self writeInteger: anObject rsrId onto: aStream].
	(self isImmediate: anObject)
		ifTrue: [^self writeImmediate: anObject onto: aStream].
	self error: 'Unsupported type'