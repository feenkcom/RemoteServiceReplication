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
	queue isEmpty
		ifTrue: [self flush]]
		on: ConnectionClosed, RsrConnectionClosed
		do: [:ex | self connection disconnected]
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
	queue flush
]

{ #category : #writing }
RsrCommandWriter >> write: aByteArray [

	self stream nextPutAll: aByteArray
]
