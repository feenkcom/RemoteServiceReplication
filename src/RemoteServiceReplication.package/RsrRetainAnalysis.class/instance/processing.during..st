processing
processing: anObject
during: aBlock

	(inFlight includes: anObject)
		ifTrue: [^RsrCycleDetected signal: 'Cycled detected on: ', anObject printString].
	inFlight add: anObject.
	self ensureRegistered: anObject.
	aBlock value.
	self retain: anObject.
	inFlight remove: anObject