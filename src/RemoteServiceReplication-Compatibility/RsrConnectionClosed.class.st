Class {
	#name : #RsrConnectionClosed,
	#superclass : #Error,
	#category : #'RemoteServiceReplication-Compatibility'
}

{ #category : #accessing }
RsrConnectionClosed >> messageText [

	^'The connection has closed'
]
