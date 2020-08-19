"
This class is abstract and defines the interface for manufacturing RsrConnection instances which are connected to a peer.

Specialized subclasses are reponsible for either listening for or initiating connections with a peer.
"
Class {
	#name : 'RsrConnectionSpecification',
	#superclass : 'RsrObject',
	#instVars : [
		'host',
		'port'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrConnectionSpecification class >> host: hostnameOrAddress
port: port [

	^self new
		host: hostnameOrAddress;
		port: port;
		yourself
]

{ #category : 'accessing' }
RsrConnectionSpecification >> host [
	"Return the configured hostname or IP address"

	^host
]

{ #category : 'accessing' }
RsrConnectionSpecification >> host: hostnameOrAddress [
	"The hostname or IP address used to establish a connection."

	host := hostnameOrAddress
]

{ #category : 'accessing' }
RsrConnectionSpecification >> port [
	"The port number used for establishing a socket"

	^port
]

{ #category : 'accessing' }
RsrConnectionSpecification >> port: aPort [
	"The port number used for establishing a socket"

	port := aPort
]

{ #category : 'accessing' }
RsrConnectionSpecification >> socketClass [
	"Return the class that should be used for creating Socket instances."

	^RsrSocket
]
