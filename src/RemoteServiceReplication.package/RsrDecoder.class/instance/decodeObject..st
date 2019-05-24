decoding
decodeObject: aStream

	| objectType |
	objectType := self decodeControlWord: aStream.
	objectType = self serviceType
		ifTrue: [^self decodeService: aStream].
	^self error: 'Invalid object'