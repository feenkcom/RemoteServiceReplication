Class {
	#name : 'RsrNullChannel',
	#superclass : 'RsrChannel',
	#category : 'RemoteServiceReplication'
}

{ #category : 'lifecycle' }
RsrNullChannel >> close [

	"NOP"
]

{ #category : 'testing' }
RsrNullChannel >> isOpen [

	^true
]

{ #category : 'lifecycle' }
RsrNullChannel >> open [

	"NOP"
]

{ #category : 'events' }
RsrNullChannel >> received: aCommand [
	"NOP"
]

{ #category : 'events' }
RsrNullChannel >> send: aCommand [
	"NOP"
]
