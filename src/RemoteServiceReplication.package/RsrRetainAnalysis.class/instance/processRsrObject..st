processing
processRsrObject: anRsrObject

	(inFlight includes: anRsrObject)
		ifTrue: [^RsrCycleDetected signal: 'Cycled detected on: ', anRsrObject printString].
	inFlight add: anRsrObject.
	self ensureRegistered: anRsrObject.
	self
		reflectedVariablesOf: anRsrObject
		do: [:each | self process: each].
	self retain: anRsrObject.
	inFlight remove: anRsrObject