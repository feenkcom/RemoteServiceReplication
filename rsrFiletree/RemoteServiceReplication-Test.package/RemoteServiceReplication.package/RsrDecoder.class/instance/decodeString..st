decoding
decodeString: aStream

	| length bytes |
	length := self nextInteger: aStream.
	bytes := aStream next: length.
	^bytes utf8Decoded