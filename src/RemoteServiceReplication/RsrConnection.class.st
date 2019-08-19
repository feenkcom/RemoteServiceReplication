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
		'dispatcher',
		'oidSpigot',
		'serviceFactory',
		'sharedNamespace',
		'log'
	],
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #'instance creation' }
RsrConnection class >> acceptOn: aPortNumber [

	| listener socket |
	listener := RsrSocket new.
	listener listenOn: aPortNumber.
	socket := listener accept.
	^(self
		socket: socket
		transactionSpigot: RsrThreadSafeNumericSpigot naturals
		oidSpigot: (RsrThreadSafeNumericSpigot start: 2 step: 1)) start
]

{ #category : #'instance creation' }
RsrConnection class >> connectTo: aPortNumber
on: aHostname [

	| socket |
	socket := RsrSocket new.
	socket
		connectTo: aPortNumber
		on: aHostname.
	^(self
		socket: socket
		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated
		oidSpigot: (RsrThreadSafeNumericSpigot start: 2 step: 1) negated) start
]

{ #category : #'instance creation' }
RsrConnection class >> connectionTimeout [

	^2
]

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
		ifFalse: [^self].
	isOpen := false.
	commandReader stop.
	commandWriter stop.
	dispatcher stop.
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

	self log info: 'Disconnected'.
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
	transactionSpigot := RsrThreadSafeNumericSpigot naturals.
	objectCache := RsrObjectCache new.
	promises := Dictionary new.
	registry := RsrRegistry reapAction: [:oid | self releaseOid: oid].
	sharedNamespace := RsrSharedNamespace new.
	log := RsrLog new
]

{ #category : #accessing }
RsrConnection >> isOpen [

	^isOpen
]

{ #category : #accessing }
RsrConnection >> log [

	^log
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

{ #category : #'public-service factory' }
RsrConnection >> serviceFor: aResponsibility [

	^serviceFactory serviceFor: aResponsibility
]

{ #category : #accessing }
RsrConnection >> sharedNamespace [

	^sharedNamespace
]

{ #category : #accessing }
RsrConnection >> socket: aSocket [

	socket := aSocket
]

{ #category : #lifecycle }
RsrConnection >> start [

	(isOpen := socket isConnected)
		ifFalse: [^RsrConnectionClosed signal].
	stream := RsrSocketStream on: socket.
	dispatcher := RsrCommandDispatcher on: self.
	commandReader := RsrCommandReader on: self.
	commandWriter := RsrCommandWriter on: self.
	dispatcher start.
	commandReader start.
	commandWriter start.
	serviceFactory := RsrServiceFactory
		rsrId: self oidSpigot next
		rsrConnection: self.
	serviceFactory registerWith: registry.
	sharedNamespace client: (self serviceFor: #RsrSharedNamespaceClient)
]

{ #category : #accessing }
RsrConnection >> stream [

	^stream
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
