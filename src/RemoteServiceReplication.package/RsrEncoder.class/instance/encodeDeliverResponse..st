private-encoding
encodeDeliverResponse: aDeliverResponse

	^ByteArray
		streamContents:
			[:stream |
			self
				writeInteger: self deliverResponseCommand
				onto: stream.
			self
				writeInteger: aDeliverResponse transaction
				onto: stream.
			self
				encodeReferenceOf: aDeliverResponse response
				onto: stream]