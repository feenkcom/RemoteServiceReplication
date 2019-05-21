processing
processRsrObject: anRsrObject

	(inFlight includes: anRsrObject)
		ifTrue: [^RsrCycleDetected signal: 'Cycled detected on: ', anRsrObject printString].
	inFlight add: anRsrObject.
	anRsrObject isMirrored
		ifFalse: [self register: anRsrObject].
	self
		reflectedVariablesOf: anRsrObject
		do: [:each | self process: each].
	anRsrObject isDirty
		ifTrue: [self retain: anRsrObject].
	inFlight remove: anRsrObject