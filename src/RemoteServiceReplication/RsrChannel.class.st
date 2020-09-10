Class {
	#name : 'RsrChannel',
	#superclass : 'RsrObject',
	#category : 'RemoteServiceReplication'
}

{ #category : 'testing' }
RsrChannel >> isOpen [
	"Report whether the Channel is open between Connections."

	^self subclassResponsibility
]
