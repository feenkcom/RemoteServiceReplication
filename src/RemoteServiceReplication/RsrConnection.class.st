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
		'channel',
		'transactionSpigot',
		'oidSpigot',
		'dispatchQueue',
		'log',
		'registry',
		'pendingMessages',
		'serviceFactory',
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
	self isOpen
		ifFalse: [self error: 'Connection is not open'].
	analysis := RsrSnapshotAnalysis
		roots: (Array with: aService), aMessage arguments
		connection: self.
	analysis perform.
	receiverReference := RsrReference from: aService.
	selectorReference := RsrReference from: aMessage selector.
	argumentReferences := aMessage arguments collect: [:each | RsrReference from: each].
	dispatchCommand := RsrSendMessage
		transaction: self transactionSpigot next
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

	channel := aChannel.
	channel connection: self
]

{ #category : 'other' }
RsrConnection >> close [

	channel close.
	self dispatchQueue stop.
	pendingMessages do: [:each | each promise error: RsrConnectionClosed new].
	pendingMessages := Dictionary new.
	registry := nil.
	closeSemaphore signal
]

{ #category : 'private-events' }
RsrConnection >> disconnected [

	self log info: 'Disconnected'.
	self close
]

{ #category : 'private-accessing' }
RsrConnection >> dispatchQueue [

	^dispatchQueue
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
	transactionSpigot := RsrThreadSafeNumericSpigot naturals.
	pendingMessages := Dictionary new.
	registry := RsrRegistry reapAction: [:oid | self releaseOid: oid].
	dispatchQueue := RsrDispatchQueue new.
	log := RsrLog new.
	closeSemaphore := Semaphore new.
]

{ #category : 'private-initialization' }
RsrConnection >> initializeServiceFactory [

	| instance |
	instance := RsrServiceFactory clientClass new.
	self ensureRegistered: instance.
	serviceFactory := instance.
	^serviceFactory
]

{ #category : 'public-testing' }
RsrConnection >> isOpen [

	^channel isOpen
]

{ #category : 'private-accessing' }
RsrConnection >> log [

	^log
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

	self dispatchQueue start.
	channel open
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

	^serviceFactory ifNil: [self initializeServiceFactory]
]

{ #category : 'public-service factory' }
RsrConnection >> serviceFor: aResponsibility [

	^self serviceFactory serviceFor: aResponsibility
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

	closeSemaphore
		wait;
		signal
]
