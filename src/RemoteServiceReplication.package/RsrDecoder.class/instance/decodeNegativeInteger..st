decoding
decodeNegativeInteger: aStream

	^(self decodeInteger: aStream) negated