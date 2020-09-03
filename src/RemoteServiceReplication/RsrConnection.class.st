"
No class-specific documentation for RsrConnection, hierarchy is:
Object
  RsrObject
    RsrConnection( isOpen transactionSpigot commandWriter commandReader registry objectCache socket stream pendingMessages dispatcher oidSpigot serviceFactory log closeSemaphore)

"
Class {
	#name : 'RsrConnection',
	#superclass : 'RsrObject',
	#instVars : [
		'isOpen',
		'transactionSpigot',
		'commandWriter',
		'commandReader',
		'registry',
		'socket',
		'stream',
		'pendingMessages',
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

	| acceptor |
	self deprecated: 'RsrConnection class>>#acceptOn: replaced by RsrAcceptConnection.'.
	acceptor := RsrAcceptConnection port: aPortNumber.
	^acceptor waitForConnection
]

{ #category : 'instance creation' }
RsrConnection class >> connectToHost: aHostname
port: aPortNumber [

	| initiator |
	self deprecated: 'RsrConnection class>>#connectToHost:port: replaced by RsrInitiateConnection.'.
	initiator := RsrInitiateConnection
		host: aHostname
		port: aPortNumber.
	^initiator connect
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
	| analysis receiverReference selectorReference argumentReferences dispatchCommand |
	isOpen
		ifFalse: [self error: 'Connection is not open'].
	analysis := RsrSnapshotAnalysis
		roots: (Array with: aService), aMessage arguments
		connection: self.
	analysis perform.
	receiverReference := RsrReference from: aService.
	selectorReference := RsrReference from: aMessage selector.
	argumentReferences := aMessage arguments collect: [:each | RsrReference from: each].
	dispatchCommand := RsrSendMessage
		transaction: self newTransactionId
		receiver: receiverReference
		selector: selectorReference
		arguments: argumentReferences.
	dispatchCommand services: analysis snapshots.
	^dispatchCommand sendOver: self
]

{ #category : 'other' }
RsrConnection >> close [

	isOpen
		ifFalse: [^self].
	isOpen := false.
	commandReader stop.
	commandWriter stop.
	dispatcher stop.
	pendingMessages do: [:each | each promise error: RsrConnectionClosed new].
	commandReader := commandWriter := dispatcher := pendingMessages  := registry := nil.
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
		ifTrue:
			[^aService _connection == self
				ifTrue: [self]
				ifFalse: [RsrAlreadyRegistered signalService: aService intendedConnection: self]].
	aService
		_id: oidSpigot next
		connection: self.
	self registry
		serviceAt: aService _id
		put: aService
]

{ #category : 'other' }
RsrConnection >> initialize [

	super initialize.
	isOpen := false.
	transactionSpigot := RsrThreadSafeNumericSpigot naturals.
	pendingMessages := Dictionary new.
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
RsrConnection >> pendingMessages [

	^pendingMessages
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
	command := RsrReleaseServices oids: (Array with: anOid).
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
