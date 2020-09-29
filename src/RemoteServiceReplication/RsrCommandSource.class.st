Class {
	#name : 'RsrCommandSource',
	#superclass : 'RsrSocketChannelLoop',
	#instVars : [
		'decoder'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'accessing' }
RsrCommandSource >> decoder [

	^RsrDecoder new
]

{ #category : 'commands' }
RsrCommandSource >> executeCycle [

	[| command |
	command := self nextCommand.
	self report: command.
	self channel received: command]
		on: RsrSocketClosed
		do:
			[:ex |
			self reportException: ex.
			self channel channelDisconnected]
]

{ #category : 'commands' }
RsrCommandSource >> nextCommand [

	^self decoder decodeCommand: self stream
]
