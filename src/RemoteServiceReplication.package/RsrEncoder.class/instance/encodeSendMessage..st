private-encoding
encodeSendMessage: aSendMessage

	^ByteArray
		streamContents:
			[:stream |
			self
				writeControlWord: self sendMessageIdentifier
				onto: stream.
			self
				writeControlWord: aSendMessage transaction
				onto: stream.
			self
				writeControlWord: aSendMessage arguments size
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