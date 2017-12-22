instance creation
getReference: anObject
	| rsr |
	rsr := RsrReference new.
	rsr put: anObject.
	^ rsr