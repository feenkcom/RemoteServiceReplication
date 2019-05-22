running
testNext

	| spigot |
	spigot := RsrNumericSpigot naturals.
	self
		assert: (Array with: 1 with: 2 with: 3)
		equals: (spigot next: 3)