initialization
getObject: rsrObjectName
	^ managedObjects at: rsrObjectName 
		ifAbsent: [ self error: 'Object not found.' ]