decoding
decodeNegativeInteger: aStream

	^(self decodeImmediateInteger: aStream) negated