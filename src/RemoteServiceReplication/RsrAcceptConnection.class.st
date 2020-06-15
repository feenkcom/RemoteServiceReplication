"
This class is responsible to listen for an incoming RsrConnection connection. Once a Socket has established, an RsrConnection is created and returned via the #connect message.

The following will wait for a connection on port 51820. Once a socket connection is accepted, it will stop listening on the provided port. The established socket is then used in the creation of an RsrConnection. The new RsrConnection is returned as a result of #connect.

| acceptor |
acceptor := RsrAcceptConnection port: 51820.
^acceptor connect
"
Class {
	#name : 'RsrAcceptConnection',
	#superclass : 'RsrConnectionSpecification',
	#instVars : [
		'port'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrAcceptConnection class >> port: aPortInteger [

	^self new
		port: aPortInteger;
		yourself
]

{ #category : 'connecting' }
RsrAcceptConnection >> connect [

	| listener socket connection |
	listener := RsrSocket new.
	[listener listenOn: self port.
	socket := listener accept]
		ensure: [listener close].
	connection := RsrConnection
		socket: socket
		transactionSpigot: RsrThreadSafeNumericSpigot naturals
		oidSpigot: RsrThreadSafeNumericSpigot naturals.
	^connection open
]

{ #category : 'accessing' }
RsrAcceptConnection >> port [

	^port
]

{ #category : 'accessing' }
RsrAcceptConnection >> port: anInteger [

	port := anInteger
]
