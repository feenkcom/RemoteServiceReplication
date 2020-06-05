Class {
	#name : 'RsrSignalErrorInAsString',
	#superclass : 'RsrObject',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'converting' }
RsrSignalErrorInAsString >> asString [

	^Error signal
]
