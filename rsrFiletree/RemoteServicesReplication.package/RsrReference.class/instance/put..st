instance creation
put: anObject
	|rsrRegistry|
	representedObject := anObject. 
	self registerAttributes.
	
	rsrRegistry := RsrRegistry uniqueInstance.
	rsrRegistry addObject: self.

	"The user provided object has had it's attributes extracted as is registered to the registry. 
	Remove the refernce to it locally so that it can be transferred across the wire."
	(anObject isKindOf: RsrReplicatableObject)
		ifFalse: [ representedObject := anObject printString ].
	
	anObject ifNil: [ representedObject := nil ].

