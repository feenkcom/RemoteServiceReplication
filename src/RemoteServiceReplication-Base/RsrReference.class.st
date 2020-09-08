"
No class-specific documentation for RsrReference, hierarchy is:
Object
  RsrObject
    RsrReference

"
Class {
	#name : 'RsrReference',
	#superclass : 'RsrObject',
	#classInstVars : [
		'referenceMapping'
	],
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'analyzing' }
RsrReference class >> analyze: anObject
using: anAnalyzer [

	^self subclassResponsibility
]

{ #category : 'instance creation' }
RsrReference class >> from: anObject [

	| referenceClass |
	referenceClass := self referenceClassFor: anObject.
	^referenceClass from: anObject
]

{ #category : 'accessing' }
RsrReference class >> referenceMapping [

	^referenceMapping ifNil: [self initializeReferenceMapping]
]

{ #category : 'accessing' }
RsrReference class >> typeIdentifier [

	^self subclassResponsibility
]

{ #category : 'accessing' }
RsrReference >> typeIdentifier [

	^self class typeIdentifier
]
