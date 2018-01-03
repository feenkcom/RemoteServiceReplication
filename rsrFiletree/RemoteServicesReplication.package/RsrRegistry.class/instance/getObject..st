initialization
getObject: rsrObjectName
	^ managedObjects at: rsrObjectName 
		ifAbsent: [ Transcript show: 'not found.' ]