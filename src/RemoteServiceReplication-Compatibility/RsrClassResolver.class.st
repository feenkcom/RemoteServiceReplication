Class {
	#name : #RsrClassResolver,
	#superclass : #Object,
	#category : #'RemoteServiceReplication-Compatibility'
}

{ #category : #accessing }
RsrClassResolver class >> classNamed: aSymbol [

	^Smalltalk at: aSymbol ifAbsent: [self error: aSymbol asString, ' was not found']
]

{ #category : #accessing }
RsrClassResolver class >> classNamed: aSymbol
ifAbsent: aBlock [

	^Smalltalk at: aSymbol ifAbsent: aBlock
]
