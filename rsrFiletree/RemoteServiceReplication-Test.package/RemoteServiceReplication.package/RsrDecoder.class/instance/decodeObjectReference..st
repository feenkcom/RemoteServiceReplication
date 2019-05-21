decoding
decodeObjectReference: aStream

	| oid |
	oid := self nextInteger: aStream.
	oid = 0 ifTrue: [^self decodeImmediateObject: aStream].
	^registry at: oid ifAbsent: [self signalUnknownOID]