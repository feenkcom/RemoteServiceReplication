running
testRemoteReferenceLifetime

	| name valueServiceLocal valueServiceRemote serviceLocal serviceRemote id marker actual |
	name := 'ValueService'.
	Transcript.
	serviceLocal := RsrClientNoInstVars new.
	valueServiceLocal := RsrValueHolderClient value: serviceLocal.
	connectionA serviceRegistry
		at: name
		put: valueServiceLocal.
	valueServiceRemote := connectionB serviceRegistry at: name.
	serviceRemote := valueServiceRemote value.
	id := serviceLocal rsrId.
	self
		assert: serviceRemote class
		equals: RsrServerNoInstVars.
	serviceLocal := serviceRemote := nil.
	valueServiceRemote value: nil.
	self maximumReclamation.
	(Delay forSeconds: 2) wait.
	Smalltalk at: #KeepItAlive put: (Array with: connectionA with: connectionB).
	marker := Object new.
	actual := connectionA registry at: id ifAbsent: [marker].
	self
		assert: actual
		equals: marker.
	actual := connectionB registry at: id ifAbsent: [marker].
	self
		assert: actual
		equals: marker