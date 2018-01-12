instance creation
put: anObject
	|rsrRegistry|
	representedObject := anObject.
	self registerAttributes.
	
	rsrRegistry := RsrRegistry uniqueInstance.
	rsrRegistry addObject: self.


