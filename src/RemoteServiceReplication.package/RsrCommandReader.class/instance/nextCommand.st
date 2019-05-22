commands
nextCommand

	| command |
	command := decoder decode: stream.
	commands := commands ifNil: [OrderedCollection new].
	^commands add: command