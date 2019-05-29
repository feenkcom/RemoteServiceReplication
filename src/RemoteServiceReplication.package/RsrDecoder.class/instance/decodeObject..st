decoding
decodeObject: aStream

	| objectType |
	objectType := self decodeControlWord: aStream.
	objectType = self serviceType
		ifTrue: [^self decodeService: aStream].
	objectType = self arrayType
		ifTrue: [^self decodeArray: aStream].
	^self error: 'Invalid object'