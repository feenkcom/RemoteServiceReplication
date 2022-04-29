Class {
	#name : 'RsrUnknownClass',
	#superclass : 'RsrError',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'temp' }
RsrUnknownClass >> signal [

	self halt.
	^super signal
]
