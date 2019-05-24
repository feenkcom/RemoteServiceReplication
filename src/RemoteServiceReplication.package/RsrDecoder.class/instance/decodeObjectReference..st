decoding
decodeObjectReference: aStream

	| oid |
	oid := self decodeControlWord: aStream.
	oid = 0 ifTrue: [^self decodeImmediateObject: aStream].
	^registry at: oid ifAbsent: [self signalUnknownOID]