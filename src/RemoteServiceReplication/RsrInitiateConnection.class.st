"
This class is responsible for initating a new RsrConnection. Sending #connect will result in an attempt to connect to the specified host and port. #connect is responsible for initating the attempted connection. If successful, an instance of RsrConnection is returned as a result.

Example: 

| initiator |
initiator := RsrInitiateConnection
	host: '127.0.0.1'
	port: 51820.
^initiator connect
"
Class {
	#name : 'RsrInitiateConnection',
	#superclass : 'RsrConnectionSpecification',
	#instVars : [
		'host',
		'port'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrInitiateConnection class >> host: aHostname
port: aPortInteger [

	^self new
		host: aHostname;
		port: aPortInteger;
		yourself
]

{ #category : 'connecting' }
RsrInitiateConnection >> connect [

	| socket connection |
	socket := RsrSocket new.
	socket
		connectToHost: self host
		port: self port.
	connection := RsrConnection
		socket: socket
		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated
		oidSpigot: RsrThreadSafeNumericSpigot naturals negated.
	^connection open
]

{ #category : 'accessing' }
RsrInitiateConnection >> host [

	^host
]

{ #category : 'accessing' }
RsrInitiateConnection >> host: aHostnameString [

	host := aHostnameString
]

{ #category : 'accessing' }
RsrInitiateConnection >> port [

	^port
]

{ #category : 'accessing' }
RsrInitiateConnection >> port: anInteger [

	port := anInteger
]
