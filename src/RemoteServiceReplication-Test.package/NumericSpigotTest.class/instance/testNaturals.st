running
testNaturals

	| spigot |
	spigot := RsrNumericSpigot naturals.
	self
		assert: spigot next
		equals: 1.
	self
		assert: spigot next
		equals: 2