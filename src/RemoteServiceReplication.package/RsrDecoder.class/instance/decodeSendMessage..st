decoding
decodeSendMessage: aStream

	| transaction argCount receiverOID receiver selector arguments |
	transaction := self nextInteger: aStream.
	argCount := self nextInteger: aStream.
	receiverOID := self nextInteger: aStream.
	receiver := registry at: receiverOID ifAbsent: [^self signalUnknownOID].
	selector := self decodeObjectReference: aStream.
	arguments := (1 to: argCount) collect: [:each | self decodeObjectReference: aStream].
	^RsrSendMessage
		transaction: transaction
		receiver: receiver
		selector: selector
		arguments: arguments