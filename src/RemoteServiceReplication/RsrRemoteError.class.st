Class {
	#name : 'RsrRemoteError',
	#superclass : 'RsrError',
	#instVars : [
		'originalClassName'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrRemoteError class >> from: anException [

	^self new
		originalClassName: anException class name;
		yourself
]

{ #category : 'accessing' }
RsrRemoteError >> messageText [

	^'an RsrRemoteError has been signaled'
]

{ #category : 'accessing' }
RsrRemoteError >> originalClassName [

	^originalClassName
]

{ #category : 'accessing' }
RsrRemoteError >> originalClassName: aSymbol [

	originalClassName := aSymbol
]
