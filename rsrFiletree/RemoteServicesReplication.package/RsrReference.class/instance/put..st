instance creation
put: anObject
	"This should get more details about the represented object.  EG. instVars."

	representedObject := anObject.
	self registerAttributes.