decoding
decodeCharacter: aStream

	| codePoint |
	codePoint := self nextInteger: aStream.
	^Character codePoint: codePoint