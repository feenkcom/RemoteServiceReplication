Class {
	#name : #RsrConnectionClosed,
	#superclass : #RsrError,
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #accessing }
RsrConnectionClosed >> messageText [

	^'The connection has closed'
]
