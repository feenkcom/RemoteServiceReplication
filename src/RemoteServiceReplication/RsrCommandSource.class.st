Class {
	#name : 'RsrCommandSource',
	#superclass : 'RsrEventLoop',
	#instVars : [
		'decoder'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'accessing' }
RsrCommandSource >> decoder [

	^self connection decoder
]

{ #category : 'accessing' }
RsrCommandSource >> dispatcher [

	^self connection dispatcher
]

{ #category : 'commands' }
RsrCommandSource >> executeCycle [

	[| command |
	command := self nextCommand.
	self report: command.
	self dispatcher dispatch: command]
		on: RsrSocketClosed
		do:
			[:ex |
			self reportException: ex.
			self connection disconnected]
]

{ #category : 'commands' }
RsrCommandSource >> nextCommand [

	^self decoder decodeCommand: self stream
]
