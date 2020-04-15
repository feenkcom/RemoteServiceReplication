Class {
	#name : 'RsrConnectionClosed',
	#superclass : 'RsrError',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'accessing' }
RsrConnectionClosed >> messageText [

	^'The connection has closed'
]
