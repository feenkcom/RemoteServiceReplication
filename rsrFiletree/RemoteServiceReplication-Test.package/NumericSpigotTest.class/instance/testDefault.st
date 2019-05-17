running
testDefault

	| spigot |
	spigot := RsrNumericSpigot new.
	self
		assert: spigot current
		equals: 0.
	self
		assert: spigot step
		equals: 1