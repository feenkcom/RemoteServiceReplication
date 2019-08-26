Class {
	#name : #RsrConnectionClosed,
	#superclass : #Error,
	#category : #'RemoteServiceReplication-Compatibility-GemStone'
}

{ #category : #accessing }
RsrConnectionClosed >> messageText [

	^'The connection has closed'
]
