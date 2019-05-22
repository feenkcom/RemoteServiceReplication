running
testNegativeStep

	| spigot |
	spigot := RsrNumericSpigot
		start: 0
		step: -1.
	self
		assert: spigot next
		equals: 0.
	self
		assert: spigot next
		equals: -1.
	self
		assert: spigot next
		equals: -2