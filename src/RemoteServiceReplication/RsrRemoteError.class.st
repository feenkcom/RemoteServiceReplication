Class {
	#name : 'RsrRemoteError',
	#superclass : 'RsrError',
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrRemoteError class >> from: anException [

	^self new
]

{ #category : 'accessing' }
RsrRemoteError >> messageText [

	^'an RsrRemoteError has been signaled'
]
