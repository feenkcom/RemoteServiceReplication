decoding
decodeCommand: aStream
	"Decode an object from the stream"

	| command |
	command := self nextInteger: aStream.
	command == self retainObjectCommand
		ifTrue:
			[^RsrRetainObject object: (self decodeObject: aStream)].
	self error: 'Invalid message'