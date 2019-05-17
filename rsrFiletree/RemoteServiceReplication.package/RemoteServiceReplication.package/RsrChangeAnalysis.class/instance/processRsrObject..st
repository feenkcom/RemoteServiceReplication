processing
processRsrObject: anRsrObject

	(inFlight includes: anRsrObject)
		ifTrue: [^self].
	inFlight add: anRsrObject.
	anRsrObject isMirrored
		ifFalse: [self register: anRsrObject].
	self
		reflectedVariablesOf: anRsrObject
		do: [:each | self process: each].
	anRsrObject isDirty
		ifTrue: [self encode: anRsrObject].
	inFlight remove: anRsrObject