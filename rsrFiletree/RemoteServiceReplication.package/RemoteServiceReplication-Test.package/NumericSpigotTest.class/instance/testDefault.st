running
testDefault

	| spigot |
	spigot := RsrNumericSpigot new.
	self
		assert: spigot next
		equals: 0.
	self
		assert: spigot step
		equals: 1