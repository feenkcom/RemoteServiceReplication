adding
add: anObject
	|rsr wrappedObject|
	rsr := self uniqueInstance.
	wrappedObject := RsrReference getReference: anObject.
	rsr addObject: wrappedObject .
	^ wrappedObject.