Class {
	#name : #NumericSpigotTest,
	#superclass : #TestCase,
	#category : 'RemoteServiceReplication-Test-TestCases'
}

{ #category : #running }
NumericSpigotTest >> testDefault [

	| spigot |
	spigot := RsrNumericSpigot new.
	self
		assert: spigot next
		equals: 0.
	self
		assert: spigot step
		equals: 1
]

{ #category : #running }
NumericSpigotTest >> testFloat [

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
]

{ #category : #running }
NumericSpigotTest >> testNaturals [

	| spigot |
	spigot := RsrNumericSpigot naturals.
	self
		assert: spigot next
		equals: 1.
	self
		assert: spigot next
		equals: 2
]

{ #category : #running }
NumericSpigotTest >> testNegativeStep [

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
]

{ #category : #running }
NumericSpigotTest >> testNext [

	| spigot |
	spigot := RsrNumericSpigot naturals.
	self
		assert: (Array with: 1 with: 2 with: 3)
		equals: (spigot next: 3)
]
