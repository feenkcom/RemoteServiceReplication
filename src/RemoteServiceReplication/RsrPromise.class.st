Class {
	#name : #RsrPromise,
	#superclass : #Object,
	#instVars : [
		'mutex',
		'value',
		'error'
	],
	#category : #'RemoteServiceReplication-Utilities'
}

{ #category : #fulfillment }
RsrPromise >> error: anException [

	error := anException.
	mutex signal

	
]

{ #category : #fulfillment }
RsrPromise >> fulfill: anObject [

	value := anObject.
	mutex signal
]

{ #category : #initialization }
RsrPromise >> initialize [

	super initialize.
	mutex := Semaphore new
]

{ #category : #accessing }
RsrPromise >> value [

	self waitForFulfillment.
	error isNil
		ifFalse: [error copy signal].
	^value
]

{ #category : #'private-waiting' }
RsrPromise >> waitForFulfillment [

	mutex isSignaled
		ifTrue: [^self].
	mutex wait.
	mutex signal
]
