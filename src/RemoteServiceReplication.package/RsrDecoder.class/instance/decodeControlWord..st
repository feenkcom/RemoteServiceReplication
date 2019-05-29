decoding
decodeControlWord: aStream

	| bytes unsignedResult |
	bytes := aStream next: self sizeOfInteger.
	unsignedResult := bytes asInteger.
	^unsignedResult > self controlWordMax
		ifTrue: [(2 raisedTo: 64) negated + unsignedResult]
		ifFalse: [unsignedResult]