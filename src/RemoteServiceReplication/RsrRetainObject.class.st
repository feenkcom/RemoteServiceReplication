Class {
	#name : #RsrRetainObject,
	#superclass : #RsrCommand,
	#instVars : [
		'object'
	],
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #'instance creation' }
RsrRetainObject class >> object: anRsrObject [

	^self new
		object: anRsrObject;
		yourself
]

{ #category : #'instance creation' }
RsrRetainObject class >> object: anRsrObject
encoding: aByteArray [

	^self new
		object: anRsrObject;
		encoding: aByteArray;
		yourself
]

{ #category : #comparing }
RsrRetainObject >> = anEncodedObject [

	self == anEncodedObject
		ifTrue: [^true].
	self class == anEncodedObject class
		ifFalse: [^false].
	^self object = anEncodedObject object
		and: [self encoding = anEncodedObject encoding]
]

{ #category : #lifecycle }
RsrRetainObject >> encodeUsing: anRsrEncoder [

	encoding := anRsrEncoder encodeRetainObject: self
]

{ #category : #lifecycle }
RsrRetainObject >> executeFor: aConnection [

	aConnection objectCache add: object
]

{ #category : #hashing }
RsrRetainObject >> hash [

	^self object hash
]

{ #category : #accessing }
RsrRetainObject >> object [
	^ object
]

{ #category : #accessing }
RsrRetainObject >> object: anObject [

	object := anObject
]

{ #category : #lifecycle }
RsrRetainObject >> writeUsing: aCommandWriter [

	super writeUsing: aCommandWriter
]
