instance creation
start: aNumber
step: anIncrement

	^super new
		current: aNumber;
		step: anIncrement;
		yourself