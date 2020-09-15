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

{ #category : 'lifecycle' }
RsrSocketChannel >> close [
	"Shutdown the Command sink and source."

	source stop.
	sink stop
]

{ #category : 'testing' }
RsrSocketChannel >> isOpen [

	^self socket isConnected
]

{ #category : 'lifecycle' }
RsrSocketChannel >> open [
	"Ensure the Command sink and source are running"

	source start.
	sink start
]

{ #category : 'command processing' }
RsrSocketChannel >> send: aCommand [
	"Send the provided command over the channel"

	sink enqueue: aCommand
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
