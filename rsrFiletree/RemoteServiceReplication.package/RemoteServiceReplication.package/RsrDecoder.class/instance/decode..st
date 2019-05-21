decoding
decode: aStream
	"Decode an object from the stream"

	| length command |
	length := self nextInteger: aStream.
	command := self nextInteger: aStream.
	command == self retainObjectCommand
		ifTrue:
			[^self
				decodeService: aStream
				ofLength: length - self sizeOfInteger].
	self error: 'Invalid message'