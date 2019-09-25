Class {
	#name : #RsrUnsupportedObject,
	#superclass : #RsrError,
	#instVars : [
		'object'
	],
	#category : #'RemoteServiceReplication-Base-Errors'
}

{ #category : #exceptioninstantiator }
RsrUnsupportedObject class >> signal: anObject [

	^self new
		object: anObject;
		signal
]

{ #category : #accessing }
RsrUnsupportedObject >> messageText [

	^'Unsupported native class (', object class name, ')'
]

{ #category : #accessing }
RsrUnsupportedObject >> object [

	^object
]

{ #category : #accessing }
RsrUnsupportedObject >> object: anObject [

	object := anObject
]
