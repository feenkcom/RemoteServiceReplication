"
This class is abstract and defines the interface for manufacturing RsrConnection instances which are connected to a peer.

Specialized subclasses are reponsible for either listening for or initiating connections with a peer.
"
Class {
	#name : 'RsrConnectionSpecification',
	#superclass : 'RsrObject',
	#category : 'RemoteServiceReplication'
}

{ #category : 'connecting' }
RsrConnectionSpecification >> connect [

	self subclassResponsibility
]

{ #category : 'accessing' }
RsrConnectionSpecification >> socketClass [
	"Return the class that should be used for creating Socket instances."

	^RsrSocket
]
