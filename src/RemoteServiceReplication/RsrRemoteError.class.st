Class {
	#name : 'RsrRemoteError',
	#superclass : 'RsrError',
	#instVars : [
		'originalClassName',
		'stack'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrRemoteError class >> from: anException [

	| tag |
	tag := anException tag.
	tag isNil
		ifFalse: [tag := tag asString].
	^self new
		originalClassName: anException class name;
		tag: tag;
		messageText: anException messageText;
		yourself
]

{ #category : 'accessing' }
RsrRemoteError >> originalClassName [

	^originalClassName
]

{ #category : 'accessing' }
RsrRemoteError >> originalClassName: aSymbol [

	originalClassName := aSymbol
]

{ #category : 'accessing' }
RsrRemoteError >> stack [

	^stack
]

{ #category : 'accessing' }
RsrRemoteError >> stack: aString [

	stack := aString
]
