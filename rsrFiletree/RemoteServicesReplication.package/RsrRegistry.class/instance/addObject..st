initialization
addObject: anRsrObject
	managedObjects    
		ifNil: [ self managedObjects ].
	managedObjects at: anRsrObject rsrName put: anRsrObject.
	


