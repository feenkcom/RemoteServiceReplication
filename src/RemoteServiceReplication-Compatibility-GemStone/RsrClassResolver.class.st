Class {
	#name : #RsrClassResolver,
	#superclass : #RsrObject,
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #accessing }
RsrClassResolver class >> classNamed: aSymbol [

	^self
		classNamed: aSymbol
		ifAbsent: [self error: 'Unknown class']
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
