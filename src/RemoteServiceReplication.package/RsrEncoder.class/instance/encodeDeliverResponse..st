private-encoding
encodeDeliverResponse: aDeliverResponse

	^ByteArray
		streamContents:
			[:stream |
			self
				encodeControlWord: self deliverResponseCommand
				onto: stream.
			self
				encodeControlWord: aDeliverResponse transaction
				onto: stream.
			self
				encodeReferenceOf: aDeliverResponse response
				onto: stream]