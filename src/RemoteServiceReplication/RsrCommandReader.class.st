Class {
	#name : #RsrCommandReader,
	#superclass : #RsrCommandProcessor,
	#instVars : [
		'decoder'
	],
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #accessing }
RsrCommandReader >> decoder [

	^self connection decoder
]

{ #category : #accessing }
RsrCommandReader >> dispatcher [

	^self connection dispatcher
]

{ #category : #commands }
RsrCommandReader >> executeCycle [

	[self dispatcher dispatch: self nextCommand]
		on: RsrSocketClosed
		do:
			[:ex |
			self log: ex.
			self connection disconnected]
]

{ #category : #commands }
RsrCommandReader >> nextCommand [

	^self decoder decodeCommand: self stream
]
