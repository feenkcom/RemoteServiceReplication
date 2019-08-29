Class {
	#name : #RsrConnectionClosed,
	#superclass : #RsrError,
	#category : #'RemoteServiceReplication-Compatibility-GemStone'
}

{ #category : #accessing }
RsrConnectionClosed >> messageText [

	^'The connection has closed'
]
