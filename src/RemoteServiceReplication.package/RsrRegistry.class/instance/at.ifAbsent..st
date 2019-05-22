initialization
at: anOID
ifAbsent: aBlock

	^map
		at: anOID
		ifAbsent: aBlock