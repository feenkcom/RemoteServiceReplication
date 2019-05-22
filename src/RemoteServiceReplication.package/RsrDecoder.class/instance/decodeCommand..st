decoding
decodeCommand: aStream
	"Decode an object from the stream"

	| command |
	command := self nextInteger: aStream.
	command == self retainObjectCommand
		ifTrue: [^RsrRetainObject object: (self decodeObject: aStream)].
	command == self sendMessageCommand
		ifTrue: [^self decodeSendMessage: aStream].
	command == self deliverResponseCommand
		ifTrue: [^self decodeDeliverResponse: aStream].
	self error: 'Invalid message'