Class {
	#name : #RsrConnectionClosed,
	#superclass : #RsrError,
	#category : #'RemoteServiceReplication-Base-Errors'
}

{ #category : #accessing }
RsrConnectionClosed >> messageText [

	^'The connection has closed'
]
