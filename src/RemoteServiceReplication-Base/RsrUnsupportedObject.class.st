Class {
	#name : 'RsrUnsupportedObject',
	#superclass : 'RsrError',
	#instVars : [
		'object'
	],
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'exceptioninstantiator' }
RsrUnsupportedObject class >> signal: anObject [

	^self new
		object: anObject;
		signal
]

{ #category : 'accessing' }
RsrUnsupportedObject >> object [

	^object
]

{ #category : 'accessing' }
RsrUnsupportedObject >> object: anObject [

	object := anObject.
	self messageText: 'Instances of ', object class name, ' cannot be serialized'
]
