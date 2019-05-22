decoding
decodeObject: aStream

	| objectType |
	objectType := self nextInteger: aStream.
	objectType = self serviceType
		ifTrue: [^self decodeService: aStream].
	^self error: 'Invalid object'