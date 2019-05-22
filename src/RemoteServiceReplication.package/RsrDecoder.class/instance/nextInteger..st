decoding
nextInteger: aStream

	| bytes |
	bytes := aStream next: self sizeOfInteger.
	^bytes asInteger