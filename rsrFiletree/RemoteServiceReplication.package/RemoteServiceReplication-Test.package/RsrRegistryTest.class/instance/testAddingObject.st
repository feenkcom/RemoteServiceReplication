tests
testAddingObject

	| object registry |
	registry := RsrRegistry new.
	object := RsrClientNoInstVars new.
	registry add: object.
	self assert: (registry includes: object)