initialization
getObject: rsrObjectName
	^ managedObjects at: rsrObjectName 
		ifAbsent: [  ]