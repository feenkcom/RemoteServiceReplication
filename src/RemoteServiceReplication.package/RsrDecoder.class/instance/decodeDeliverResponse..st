decoding
decodeDeliverResponse: aStream

	| transaction response |
	transaction := self nextInteger: aStream.
	response := self decodeObjectReference: aStream.
	^RsrDeliverResponse
		transaction: transaction
		response: response