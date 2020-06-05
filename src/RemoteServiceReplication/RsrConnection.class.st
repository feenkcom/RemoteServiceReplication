Class {
	#name : 'RsrConnection',
	#superclass : 'RsrObject',
	#instVars : [
		'isOpen',
		'transactionSpigot',
		'commandWriter',
		'commandReader',
		'registry',
		'objectCache',
		'socket',
		'stream',
		'promises',
		'dispatcher',
		'oidSpigot',
		'serviceFactory',
		'log',
		'closeSemaphore'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrConnection class >> acceptOn: aPortNumber [

	| listener socket |
	listener := RsrSocket new.
	[listener listenOn: aPortNumber.
	socket := listener accept]
		ensure: [listener close].
	^(self
		socket: socket
		transactionSpigot: RsrThreadSafeNumericSpigot naturals
		oidSpigot: RsrThreadSafeNumericSpigot naturals) open
]

{ #category : 'instance creation' }
RsrConnection class >> connectToHost: aHostname
port: aPortNumber [

	| socket |
	socket := RsrSocket new.
	socket
		connectToHost: aHostname
		port: aPortNumber.
	^(self
		socket: socket
		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated
		oidSpigot: RsrThreadSafeNumericSpigot naturals negated) open
]

{ #category : 'instance creation' }
RsrConnection class >> new [

	self error: 'Instance creation via #new is unsupported'
]

{ #category : 'instance creation' }
RsrConnection class >> socket: aSocket
transactionSpigot: aNumericSpigot
oidSpigot: anOidSpigot [

	^super new
		socket: aSocket;
		transactionSpigot: aNumericSpigot;
		oidSpigot: anOidSpigot;
		yourself
]

{ #category : 'private-accessing' }
RsrConnection >> _forwarderClass [

	^RsrForwarder
]

{ #category : 'private-sending' }
RsrConnection >> _sendMessage: aMessage
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

{ #category : 'public-lifecycle' }
RsrConnection >> close [

	isOpen
		ifFalse: [^self].
	isOpen := false.
	commandReader stop.
	commandWriter stop.
	dispatcher stop.
	self promises do: [:each | each error: RsrConnectionClosed new].
	objectCache reset.
	closeSemaphore signal
]

{ #category : 'private-accessing' }
RsrConnection >> commandReader [

	^commandReader
]

{ #category : 'private-accessing' }
RsrConnection >> commandWriter [

	^commandWriter
]

{ #category : 'private-accessing' }
RsrConnection >> decoder [

	^RsrDecoder registry: registry connection: self
]

{ #category : 'private-events' }
RsrConnection >> disconnected [

	self log info: 'Disconnected'.
	self close
]

{ #category : 'private-accessing' }
RsrConnection >> dispatcher [

	^dispatcher
]

{ #category : 'private-accessing' }
RsrConnection >> encoder [

	^RsrEncoder new
]

{ #category : 'private-registration' }
RsrConnection >> ensureRegistered: aService [

	aService isMirrored
		ifTrue: [^self].
	aService
		_id: oidSpigot next
		connection: self.
	self registry
		serviceAt: aService _id
		put: aService
]

{ #category : 'private-initialization' }
RsrConnection >> initialize [

	super initialize.
	isOpen := false.
	transactionSpigot := RsrThreadSafeNumericSpigot naturals.
	objectCache := RsrObjectCache new.
	promises := Dictionary new.
	registry := RsrRegistry reapAction: [:oid | self releaseOid: oid].
	log := RsrLog new
]

{ #category : 'public-testing' }
RsrConnection >> isClosed [

	^self isOpen not
]

{ #category : 'public-testing' }
RsrConnection >> isOpen [

	^isOpen
]

{ #category : 'private-accessing' }
RsrConnection >> log [

	^log
]

{ #category : 'private-accessing' }
RsrConnection >> newTransactionId [

	^transactionSpigot next
]

{ #category : 'private-accessing' }
RsrConnection >> objectCache [

	^objectCache
]

{ #category : 'private-accessing' }
RsrConnection >> oidSpigot [

	^oidSpigot
]

{ #category : 'private-accessing' }
RsrConnection >> oidSpigot: anIntegerSpigot [

	oidSpigot := anIntegerSpigot
]

{ #category : 'public-lifecycle' }
RsrConnection >> open [

	(isOpen := socket isConnected)
		ifFalse: [^RsrConnectionClosed signal].
	closeSemaphore := Semaphore new.
	stream := RsrSocketStream on: socket.
	dispatcher := RsrDispatchEventLoop on: self.
	commandReader := RsrCommandSource on: self.
	commandWriter := RsrCommandSink on: self.
	dispatcher start.
	commandReader start.
	commandWriter start.
	serviceFactory := RsrServiceFactory clientClass
		_id: self oidSpigot next
		connection: self.
	registry
		serviceAt: serviceFactory _id
		put: serviceFactory
]

{ #category : 'private-accessing' }
RsrConnection >> promises [

	^promises
]

{ #category : 'private-accessing' }
RsrConnection >> registry [

	^registry
]

{ #category : 'private-coordination' }
RsrConnection >> releaseOid: anOid [

	| command |
	self isOpen
		ifFalse: [^self].
	self log trace: 'Cleaning up OID:', anOid printString.
	command := RsrReleaseObjects oids: (Array with: anOid).
	command encodeUsing: self encoder.
	commandWriter enqueue: command
]

{ #category : 'public-service factory' }
RsrConnection >> serviceFactory [

	^serviceFactory
]

{ #category : 'public-service factory' }
RsrConnection >> serviceFor: aResponsibility [

	^self serviceFactory serviceFor: aResponsibility
]

{ #category : 'private-accessing' }
RsrConnection >> socket: aSocket [

	socket := aSocket
]

{ #category : 'private-accessing' }
RsrConnection >> stream [

	^stream
]

{ #category : 'private-accessing' }
RsrConnection >> transactionSpigot [

	^transactionSpigot
]

{ #category : 'private-accessing' }
RsrConnection >> transactionSpigot: anObject [

	transactionSpigot := anObject
]

{ #category : 'private-events' }
RsrConnection >> unknownError: anException [

	self close
]

{ #category : 'public-waiting' }
RsrConnection >> waitUntilClose [

	closeSemaphore wait.
	closeSemaphore signal
]
