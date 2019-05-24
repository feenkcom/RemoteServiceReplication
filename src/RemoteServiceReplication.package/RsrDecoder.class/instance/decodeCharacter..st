decoding
decodeCharacter: aStream

	| codePoint |
	codePoint := self decodeControlWord: aStream.
	^Character codePoint: codePoint