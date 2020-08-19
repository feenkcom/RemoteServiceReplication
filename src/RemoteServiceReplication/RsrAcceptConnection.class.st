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
		'listener',
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

{ #category : 'cancelling' }
RsrAcceptConnection >> cancelWaitForConnection [

	listener ifNotNil: [:socket | socket close]
]

{ #category : 'testing' }
RsrAcceptConnection >> isWaitingForConnection [

	^listener ~~ nil
]

{ #category : 'accessing' }
RsrAcceptConnection >> port [

	^port
]

{ #category : 'accessing' }
RsrAcceptConnection >> port: anInteger [

	port := anInteger
]

{ #category : 'connecting' }
RsrAcceptConnection >> waitForConnection [

	| socket connection |
	listener := self socketClass new.
	[listener
		bindAddress: '0.0.0.0'
		port: self port.
	listener listen: 1.
	socket := [listener accept]
		on: RsrSocketClosed
		do: [:ex | ex resignalAs: RsrWaitForConnectionCancelled new]]
			ensure:
				[listener close.
				listener := nil].
	connection := RsrConnection
		socket: socket
		transactionSpigot: RsrThreadSafeNumericSpigot naturals
		oidSpigot: RsrThreadSafeNumericSpigot naturals.
	^connection open
]
