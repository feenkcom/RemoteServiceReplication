Class {
	#name : #RsrConnection,
	#superclass : #Object,
	#instVars : [
		'isOpen',
		'forwarderClass',
		'transactionSpigot',
		'commandWriter',
		'commandReader',
		'registry',
		'objectCache',
		'socket',
		'stream',
		'promises',
		'serviceRegistry',
		'dispatcher',
		'reaper',
		'oidSpigot'
	],
	#category : 'RemoteServiceReplication-Communications'
}

{ #category : #'instance creation' }
RsrConnection class >> new [

	self error
]

{ #category : #'instance creation' }
RsrConnection class >> socket: aSocket
transactionSpigot: aNumericSpigot
oidSpigot: anOidSpigot [

	^super new
		socket: aSocket;
		transactionSpigot: aNumericSpigot;
		oidSpigot: anOidSpigot;
		yourself
]

{ #category : #lifecycle }
RsrConnection >> close [

	isOpen
		ifFalse: [ ^ self ].
	isOpen := false.
	commandReader stop.
	commandWriter stop.
	self promises do: [:each | each error: RsrConnectionClosed new].
	objectCache reset
]

{ #category : #accessing }
RsrConnection >> commandReader [

	^commandReader
]

{ #category : #accessing }
RsrConnection >> commandReader: aCommandReader [

	commandReader := aCommandReader
]

{ #category : #accessing }
RsrConnection >> commandWriter [

	^commandWriter
]

{ #category : #accessing }
RsrConnection >> commandWriter: anRsrCommandWriter [

	commandWriter := anRsrCommandWriter
]

{ #category : #accessing }
RsrConnection >> decoder [

	^RsrDecoder registry: registry connection: self
]

{ #category : #'private-events' }
RsrConnection >> disconnected [

	self close
]

{ #category : #accessing }
RsrConnection >> dispatcher [

	^dispatcher
]

{ #category : #accessing }
RsrConnection >> encoder [

	^RsrEncoder new
]

{ #category : #accessing }
RsrConnection >> forwarderClass [

	^forwarderClass ifNil: [RsrForwarder]
]

{ #category : #accessing }
RsrConnection >> forwarderClass: anObject [

	forwarderClass := anObject
]

{ #category : #initialization }
RsrConnection >> initialize [

	super initialize.
	isOpen := false.
	reaper := RsrObjectReaper connection: self.
	transactionSpigot := RsrThreadSafeNumericSpigot naturals.
	objectCache := RsrObjectCache new.
	promises := Dictionary new.
	registry := RsrRegistry new.
	serviceRegistry := RsrServiceRegistry
		rsrId: 1
		rsrConnection: self.
	registry retain: serviceRegistry
]

{ #category : #accessing }
RsrConnection >> isOpen [

	^isOpen
]

{ #category : #'private-accessing' }
RsrConnection >> newTransactionId [

	^transactionSpigot next
]

{ #category : #accessing }
RsrConnection >> objectCache [

	^objectCache
]

{ #category : #accessing }
RsrConnection >> oidSpigot [

	^oidSpigot
]

{ #category : #accessing }
RsrConnection >> oidSpigot: anIntegerSpigot [

	oidSpigot := anIntegerSpigot
]

{ #category : #accessing }
RsrConnection >> promises [

	^promises
]

{ #category : #accessing }
RsrConnection >> reaper [

	^reaper
]

{ #category : #accessing }
RsrConnection >> registry [

	^registry
]

{ #category : #'private-coordination' }
RsrConnection >> releaseOid: anOid [

	| command |
	self isOpen
		ifFalse: [^self].
	command := RsrReleaseObjects oids: (Array with: anOid).
	command encodeUsing: self encoder.
	commandWriter enqueue: command
]

{ #category : #'private-accessing' }
RsrConnection >> rsrForwarderClass [

	^self forwarderClass
]

{ #category : #'private-accessing' }
RsrConnection >> rsrForwarderClass: aClass [

	^self forwarderClass: aClass
]

{ #category : #'private-sending' }
RsrConnection >> rsrSendMessage: aMessage
to: aService [

"Open coordination window"
	"Send dirty transitive closure of aRemoteMessage"
	"Send DispatchMessage command"
"Coorination window closed"
	"Return Promise"
	| dispatchCommand |
	isOpen
		ifFalse: [self error: 'Connection is not open'].
	dispatchCommand := RsrSendMessage
		transaction: self newTransactionId
		receiver: aService
		selector: aMessage selector
		arguments: aMessage arguments.
	^dispatchCommand sendOver: self
]

{ #category : #accessing }
RsrConnection >> serviceRegistry [

	^serviceRegistry
]

{ #category : #accessing }
RsrConnection >> socket: aSocket [

	socket := aSocket
]

{ #category : #lifecycle }
RsrConnection >> start [

	(isOpen := socket isConnected)
		ifFalse: [^RsrConnectionClosed signal].
	stream := RsrStream on: (SocketStream on: socket).
	stream binary.
	serviceRegistry registerWith: registry.
	dispatcher := RsrCommandDispatcher on: self.
	commandReader := RsrCommandReader on: self.
	commandWriter := RsrCommandWriter on: self.
	dispatcher start.
	commandReader start.
	commandWriter start
]

{ #category : #accessing }
RsrConnection >> stream [

	^stream ifNil: [stream := RsrStream on: (SocketStream on: socket)]
]

{ #category : #accessing }
RsrConnection >> transactionSpigot [

	^transactionSpigot
]

{ #category : #accessing }
RsrConnection >> transactionSpigot: anObject [

	transactionSpigot := anObject
]

{ #category : #'private-events' }
RsrConnection >> unknownError: anException [

	self close
]
