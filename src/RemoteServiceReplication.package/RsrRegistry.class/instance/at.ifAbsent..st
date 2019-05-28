initialization
at: anOID
ifAbsent: aBlock

	^mutex
		critical:
			[map
				at: anOID
				ifAbsent: aBlock]