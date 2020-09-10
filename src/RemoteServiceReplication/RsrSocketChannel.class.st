"
No class-specific documentation for RsrSocketChannel, hierarchy is:
Object
  RsrObject
    RsrChannel
      RsrSocketChannel( reader writer socket stream)

"
Class {
	#name : 'RsrSocketChannel',
	#superclass : 'RsrChannel',
	#instVars : [
		'sink',
		'source',
		'socket',
		'stream'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'testing' }
RsrSocketChannel >> isOpen [

	^self socket isConnected
]

{ #category : 'sending' }
RsrSocketChannel >> send: aCommand [

	self sink enqueue: aCommand
]

{ #category : 'accessing' }
RsrSocketChannel >> sink [

	^sink
]

{ #category : 'accessing' }
RsrSocketChannel >> sink: aCommandSink [

	sink := aCommandSink
]

{ #category : 'accessing' }
RsrSocketChannel >> socket [

	^socket
]

{ #category : 'accessing' }
RsrSocketChannel >> socket: aSocket [

	socket := aSocket
]

{ #category : 'accessing' }
RsrSocketChannel >> source [

	^source
]

{ #category : 'accessing' }
RsrSocketChannel >> source: aCommandSource [

	source := aCommandSource
]

{ #category : 'accessing' }
RsrSocketChannel >> stream [

	^stream
]

{ #category : 'accessing' }
RsrSocketChannel >> stream: aStream [

	stream := aStream
]
