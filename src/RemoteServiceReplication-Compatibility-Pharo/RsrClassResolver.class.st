Class {
	#name : #RsrClassResolver,
	#superclass : #RsrObject,
	#category : #'RemoteServiceReplication-Compatibility-Pharo-RemoteServiceReplication-Compatibility-Pharo'
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
