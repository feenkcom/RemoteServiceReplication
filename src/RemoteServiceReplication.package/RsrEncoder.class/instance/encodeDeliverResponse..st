private-encoding
encodeDeliverResponse: aDeliverResponse

	^ByteArray
		streamContents:
			[:stream |
			self
				writeControlWord: self deliverResponseCommand
				onto: stream.
			self
				writeControlWord: aDeliverResponse transaction
				onto: stream.
			self
				encodeReferenceOf: aDeliverResponse response
				onto: stream]