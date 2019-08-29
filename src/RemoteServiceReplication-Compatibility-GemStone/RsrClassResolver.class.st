Class {
	#name : #RsrClassResolver,
	#superclass : #RsrObject,
	#category : #'RemoteServiceReplication-Compatibility-GemStone'
}

{ #category : #accessing }
RsrClassResolver class >> classNamed: aSymbol [

	^self
		classNamed: aSymbol
		ifAbsent: [RsrUnknownClass signal: aSymbol]
]

{ #category : #accessing }
RsrClassResolver class >> classNamed: aSymbol
ifAbsent: aBlock [

	| assoc |
	assoc := System myUserProfile resolveSymbol: aSymbol.
	^assoc
		ifNil: aBlock
		ifNotNil: [assoc value]
]
