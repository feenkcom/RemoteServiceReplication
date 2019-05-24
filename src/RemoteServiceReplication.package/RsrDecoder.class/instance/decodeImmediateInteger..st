decoding
decodeImmediateInteger: aStream

	| length bytes |
	length := self decodeControlWord: aStream.
	bytes := aStream next: length.
	^bytes asInteger