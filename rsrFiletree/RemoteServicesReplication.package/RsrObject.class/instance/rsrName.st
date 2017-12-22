identity
rsrName
	^ rsrName 
		ifNil: [ rsrName := representedObject class printString, RsrRegistry objectOffset printString. ]