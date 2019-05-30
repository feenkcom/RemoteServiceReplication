processing
processRsrCollection: aCollection

	(inFlight includes: aCollection)
		ifTrue: [^RsrCycleDetected signal: 'Cycled detected on: ', aCollection printString].
	inFlight add: aCollection.
	self ensureRegistered: aCollection.
	aCollection do: [:each | self process: each].
	self retain: aCollection.
	inFlight remove: aCollection