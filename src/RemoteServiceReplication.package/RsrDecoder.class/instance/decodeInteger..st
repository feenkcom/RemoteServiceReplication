decoding
decodeInteger: aStream

	| length bytes |
	length := self nextInteger: aStream.
	bytes := aStream next: length.
	^bytes asInteger