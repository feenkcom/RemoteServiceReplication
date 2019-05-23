commands
nextCommand

	| command |
	command := self decoder decodeCommand: self stream.
	commands := commands ifNil: [OrderedCollection new].
	^commands add: command