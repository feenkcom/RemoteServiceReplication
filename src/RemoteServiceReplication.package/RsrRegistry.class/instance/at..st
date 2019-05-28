initialization
at: anOID

	^mutex critical: [map at: anOID]