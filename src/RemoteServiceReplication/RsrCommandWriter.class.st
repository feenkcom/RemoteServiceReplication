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

	[| item |
	item := queue next.
	item == self stopToken
		ifTrue: [^self].
	item writeUsing: self.
	(queue size = 0)
		ifTrue: [self flush]]
		on: RsrConnectionClosed
		do:
			[:ex |
			self log: ex.
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
