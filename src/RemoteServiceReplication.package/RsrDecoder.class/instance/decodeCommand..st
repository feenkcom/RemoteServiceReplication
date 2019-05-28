decoding
decodeCommand: aStream
	"Decode an object from the stream"

	| command |
	command := self decodeControlWord: aStream.
	command == self retainObjectCommand
		ifTrue: [^RsrRetainObject object: (self decodeObject: aStream)].
	command == self sendMessageCommand
		ifTrue: [^self decodeSendMessage: aStream].
	command == self deliverResponseCommand
		ifTrue: [^self decodeDeliverResponse: aStream].
	command == self releaseObjectsCommand
		ifTrue: [^self decodeReleaseObjects: aStream].
	self error: 'Invalid message'