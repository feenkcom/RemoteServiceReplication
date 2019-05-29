processing
process: anObject

	(self isDataObject: anObject)
		ifTrue: [^self processDataObject: anObject].
	(self isRsrCollection: anObject)
		ifTrue: [^self processRsrCollection: anObject].
	(self isRsrObject: anObject)
		ifTrue: [^self processRsrObject: anObject].
	(self isImmediate: anObject)
		ifTrue: [^anObject].
	^RsrUnsupportedObject signal: 'Unsupported object (' , anObject printString , ')'