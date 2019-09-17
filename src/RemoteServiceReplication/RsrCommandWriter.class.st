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

	isRunning ifTrue: [queue nextPut: aCommand]
]

{ #category : #commands }
RsrCommandWriter >> executeCycle [

	[queue next writeUsing: self.
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

{ #category : #writing }
RsrCommandWriter >> write: aByteArray [

	self stream nextPutAll: aByteArray
]
