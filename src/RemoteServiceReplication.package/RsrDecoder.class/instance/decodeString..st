decoding
decodeString: aStream

	| length bytes |
	length := self decodeControlWord: aStream.
	bytes := aStream next: length.
	^bytes utf8Decoded