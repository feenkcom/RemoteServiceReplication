Class {
	#name : #RsrPromise,
	#superclass : #RsrObject,
	#instVars : [
		'mutex',
		'value',
		'error',
		'markerValue'
	],
	#category : #'RemoteServiceReplication-Utilities'
}

{ #category : #fulfillment }
RsrPromise >> error: anException [

	self isFulfilled
		ifTrue: [^self error: 'Promise value already set'].
	error := anException.
	mutex signal

	
]

{ #category : #fulfillment }
RsrPromise >> fulfill: anObject [

	self isFulfilled
		ifTrue: [^self error: 'Promise value already set'].
	value := anObject.
	mutex signal
]

{ #category : #initialization }
RsrPromise >> initialize [

	super initialize.
	value := markerValue := Object new.
	mutex := Semaphore new
]

{ #category : #'private-waiting' }
RsrPromise >> isFulfilled [

	^value ~~ markerValue
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

	self isFulfilled
		ifTrue: [^self].
	mutex wait.
	mutex signal
]
