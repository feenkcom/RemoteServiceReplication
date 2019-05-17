running
testFloat

	| spigot |
	spigot := RsrNumericSpigot
		start: 0
		step: 0.5.
	self
		assert: spigot next
		equals: 0.
	self
		assert: spigot next
		equals: 0.5.
	self
		assert: spigot next
		equals: 1.0.