private-encoding
encodeSendMessage: aSendMessage

	^ByteArray
		streamContents:
			[:stream |
			self
				writeInteger: self sendMessageIdentifier
				onto: stream.
			self
				writeInteger: aSendMessage transaction
				onto: stream.
			self
				writeInteger: aSendMessage arguments size
				onto: stream.
			self
				encodeReferenceOf: aSendMessage receiver
				onto: stream.
			self
				encodeReferenceOf: aSendMessage selector
				onto: stream.
			aSendMessage arguments
				do:
					[:each |
					self
						encodeReferenceOf: each
						onto: stream]]