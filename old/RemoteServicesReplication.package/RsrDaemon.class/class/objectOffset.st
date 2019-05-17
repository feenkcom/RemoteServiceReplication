instance creation
objectOffset
	^objectOffset
		ifNil: [ objectOffset := 0. ]
		ifNotNil: [ objectOffset := objectOffset + 1. ]
	
