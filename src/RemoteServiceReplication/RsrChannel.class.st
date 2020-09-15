Class {
	#name : 'RsrChannel',
	#superclass : 'RsrObject',
	#category : 'RemoteServiceReplication'
}

{ #category : 'lifecycle' }
RsrChannel >> close [
	"Ensure the channel is closed to further communication."

	^self subclassResponsibility
]

{ #category : 'testing' }
RsrChannel >> isOpen [
	"Report whether the Channel is open between Connections."

	^self subclassResponsibility
]

{ #category : 'lifecycle' }
RsrChannel >> open [
	"Ensure the channel is open and ready for communication."

	^self subclassResponsibility
]

{ #category : 'command processing' }
RsrChannel >> send: aCommand [
	"Send the provided command over the channel."

	^self subclassResponsibility
]
