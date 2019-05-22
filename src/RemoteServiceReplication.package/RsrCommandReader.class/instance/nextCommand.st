commands
nextCommand

	| command |
	command := decoder decode: stream.
	^command