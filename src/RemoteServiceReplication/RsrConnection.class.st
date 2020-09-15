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
		'registry',
		'pendingMessages',
		'dispatcher',
		'oidSpigot',
		'serviceFactory',
		'log',
		'closeSemaphore',
		'channel'
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
RsrConnection class >> channel: aChannel
transactionSpigot: aNumericSpigot
oidSpigot: anOidSpigot [

	^super new
		channel: aChannel;
		transactionSpigot: aNumericSpigot;
		oidSpigot: anOidSpigot;
		yourself
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

	| channel |
	channel := RsrSocketChannel new.
	channel socket: aSocket.
	^self
		channel: channel
		transactionSpigot: aNumericSpigot
		oidSpigot: anOidSpigot
]

{ #category : 'private-accessing' }
RsrConnection >> _forwarderClass [

	^RsrForwarder
]

{ #category : 'private-sending' }
RsrConnection >> _sendCommand: aCommand [

	channel send: aCommand
]

{ #category : 'private-sending' }
RsrConnection >> _sendMessage: aMessage
to: aService [

"Open coordination window"
	"Send dirty transitive closure of aRemoteMessage"
	"Send DispatchMessage command"
"Coorination window closed"
	"Return Promise"
	| analysis receiverReference selectorReference argumentReferences dispatchCommand promise pendingMessage |
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
	dispatchCommand snapshots: analysis snapshots.
	promise := RsrPromise new.
	pendingMessage := RsrPendingMessage
		services: nil "I don't think we need to cache services here. They will remain on the stack unless they were removed from the transitive closure by another proc"
		promise: promise.
	self pendingMessages
		at: dispatchCommand transaction
		put: pendingMessage.
	self _sendCommand: dispatchCommand.
	^promise
]

{ #category : 'private-accessing' }
RsrConnection >> channel [

	^channel
]

{ #category : 'private-accessing' }
RsrConnection >> channel: aChannel [

	channel := aChannel
]

{ #category : 'other' }
RsrConnection >> close [

	isOpen
		ifFalse: [^self].
	isOpen := false.
	channel close.
	self dispatcher stop.
	pendingMessages do: [:each | each promise error: RsrConnectionClosed new].
	channel := dispatcher := pendingMessages  := registry := nil.
	closeSemaphore signal
]

{ #category : 'private-accessing' }
RsrConnection >> decoder [

	^RsrDecoder registry: registry
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

{ #category : 'other' }
RsrConnection >> open [

	(isOpen := channel socket isConnected)
		ifFalse: [^RsrConnectionClosed signal].
	closeSemaphore := Semaphore new.
	dispatcher := RsrDispatchEventLoop on: self.
	channel stream: (RsrSocketStream on: channel socket).
	channel source: (RsrCommandSource on: self).
	channel sink: (RsrCommandSink on: self).
	self dispatcher start.
	channel open.
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
	command := RsrReleaseServices sids: (Array with: anOid).
	self _sendCommand: command
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
RsrConnection >> stream [

	^channel stream
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
