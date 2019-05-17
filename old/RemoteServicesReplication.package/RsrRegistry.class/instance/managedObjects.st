initialization
managedObjects
	^managedObjects 
		ifNil: [ managedObjects := Dictionary new ]