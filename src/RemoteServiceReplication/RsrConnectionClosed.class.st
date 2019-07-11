Class {
	#name : #RsrConnectionClosed,
	#superclass : #RsrError,
	#category : #'RemoteServiceReplication-Exceptions'
}

{ #category : #accessing }
RsrConnectionClosed >> messageText [

	^'The connection has closed'
]
