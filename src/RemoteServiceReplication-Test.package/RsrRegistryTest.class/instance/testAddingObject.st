tests
testAddingObject

	| object registry |
	registry := RsrRegistry new.
	object := RsrServerNoInstVars
		rsrId: 1
		rsrConnection: RsrMockConnection new.
	registry retain: object.
	self assert: (registry includes: object)