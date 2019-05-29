decoding
decodeArray: aStream

	| oid size array |
	oid := self decodeControlWord: aStream.
	size := self decodeControlWord: aStream.
	array := registry
		at: oid
		ifAbsent:
			[(RsrArray new: size)
				rsrId: oid;
				rsrConnection: self connection;
				markForFinalization;
				yourself].
	self registry remember: array.
	1 to: size do: [:i | array at: i put: (self decodeObjectReference: aStream)].
	^array