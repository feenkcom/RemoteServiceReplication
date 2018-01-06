instance creation
put: anObject
	|rr|
	rr := RsrRegistry uniqueInstance.
	representedObject := anObject.
	self registerAttributes.
	rr addObject: self.
