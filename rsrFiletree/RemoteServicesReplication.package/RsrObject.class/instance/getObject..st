initialization
getObject: anObjectName
	| rsrRegistry |
	rsrRegistry := RsrRegistry uniqueInstance.
	representedObject := rsrRegistry getObject: anObjectName