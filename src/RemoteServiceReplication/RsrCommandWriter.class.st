Class {
	#name : #RsrCommandWriter,
	#superclass : #RsrCommandProcessor,
	#instVars : [
		'queue'
	],
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #commands }
RsrCommandWriter >> enqueue: aCommand [

	self isActive ifTrue: [queue nextPut: aCommand]
]

{ #category : #commands }
RsrCommandWriter >> executeCycle [

	[| command |
	command := queue next.
	command == self stopToken
		ifTrue: [^self].
	self writeCommand: command.
	(queue size = 0)
		ifTrue: [self flush]]
		on: RsrSocketClosed
		do:
			[:ex |
			self reportException: ex.
			self connection disconnected]
]

{ #category : #commands }
RsrCommandWriter >> flush [

	self stream flush
]

{ #category : #initialization }
RsrCommandWriter >> initialize [

	super initialize.
	queue := SharedQueue new
]

{ #category : #commands }
RsrCommandWriter >> stop [

	super stop.
	queue nextPut: self stopToken
]

{ #category : #accessing }
RsrCommandWriter >> stopToken [

	^self stoppedState
]

{ #category : #writing }
RsrCommandWriter >> write: aByteArray [

	self stream nextPutAll: aByteArray
]

{ #category : #writing }
RsrCommandWriter >> writeCommand: aCommand [

	self report: aCommand.
	aCommand writeUsing: self
]
