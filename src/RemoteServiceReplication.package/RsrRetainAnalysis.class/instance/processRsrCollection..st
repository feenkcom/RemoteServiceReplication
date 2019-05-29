processing
processRsrCollection: aCollection

	(inFlight includes: aCollection)
		ifTrue: [^RsrCycleDetected signal: 'Cycled detected on: ', aCollection printString].
	inFlight add: aCollection.
	aCollection isMirrored
		ifFalse: [self register: aCollection].
	aCollection do: [:each | self process: each].
	aCollection isDirty
		ifTrue: [self retain: aCollection].
	inFlight remove: aCollection