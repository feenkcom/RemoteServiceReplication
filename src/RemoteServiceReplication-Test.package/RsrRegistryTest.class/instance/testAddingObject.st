tests
testAddingObject

	| object registry |
	registry := RsrRegistry oidSpigot: RsrNumericSpigot naturals.
	object := RsrClientNoInstVars new.
	registry retain: object.
	self assert: (registry includes: object)