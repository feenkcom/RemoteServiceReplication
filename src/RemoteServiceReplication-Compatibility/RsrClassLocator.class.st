Class {
	#name : #RsrClassLocator,
	#superclass : #Object,
	#category : #'RemoteServiceReplication-Compatibility'
}

{ #category : #accessing }
RsrClassLocator class >> classNamed: aSymbol [

	^Smalltalk at: aSymbol ifAbsent: [self error: aSymbol asString, ' was not found']
]

{ #category : #accessing }
RsrClassLocator class >> classNamed: aSymbol
ifAbsent: aBlock [

	^Smalltalk at: aSymbol ifAbsent: aBlock
]
