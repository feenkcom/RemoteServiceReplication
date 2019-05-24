decoding
decodeDeliverResponse: aStream

	| transaction response |
	transaction := self decodeControlWord: aStream.
	response := self decodeObjectReference: aStream.
	^RsrDeliverResponse
		transaction: transaction
		response: response