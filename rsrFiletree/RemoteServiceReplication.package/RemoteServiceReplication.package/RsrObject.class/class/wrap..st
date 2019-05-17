instance creation
wrap: anObject
	|rsr|
	rsr := RsrObject new.
	rsr put: anObject.
	^rsr