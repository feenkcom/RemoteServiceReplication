"
This class is intended to emulate a Dictionary containing strong keys and weak values which are finalized.

When a value stored in the dictionary and is not referenced elsewhere, the object should be finalized.
"
Class {
	#name : #RsrDictionary,
	#superclass : #Dictionary,
	#instVars : [
		'reapAction'
	],
	#category : #'RemoteServiceReplication-Compatibility'
}

{ #category : #'instance creation' }
RsrDictionary class >> new [

	^self onReap: [:value |]
]

{ #category : #'instance creation' }
RsrDictionary class >> onReap: aBlock [

	^super new
		reapAction: aBlock;
		yourself
]

{ #category : #accessing }
RsrDictionary >> at: aKey [

	^self at: aKey ifAbsent: [self error: 'Key not found']
]

{ #category : #accessing }
RsrDictionary >> at: aKey
ifAbsent: aBlock [

	| result |
	result := super
		at: aKey
		ifAbsent: [nil].
	^result
		ifNil: aBlock
		ifNotNil: [:element | element object]
]

{ #category : #accessing }
RsrDictionary >> at: aKey
put: anObject [

	| element |
	element := RsrDictionaryElement object: anObject.
	anObject
		toFinalizeSend: #reap:
		to: self
		with: aKey.
	super
		at: aKey
		put: element.
	^anObject
]

{ #category : #accessing }
RsrDictionary >> reap: key [

	self removeKey: key ifAbsent: [].
	self reapAction value: key
]

{ #category : #accessing }
RsrDictionary >> reapAction [

	^reapAction
]

{ #category : #accessing }
RsrDictionary >> reapAction: aBlock [

	reapAction := aBlock
]
