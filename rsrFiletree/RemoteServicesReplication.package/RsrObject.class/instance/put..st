instance creation
put: anObject 
	representedObject := anObject.
	self rsrName.
	self rsrSerialNumber.
	"When this object gets GC'd we need to clean up remote references to it"
	self class finalizationRegistry add: self.