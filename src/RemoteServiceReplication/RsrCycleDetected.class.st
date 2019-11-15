Class {
	#name : #RsrCycleDetected,
	#superclass : #RsrError,
	#instVars : [
		'object'
	],
	#category : #'RemoteServiceReplication-Exceptions'
}

{ #category : #signaling }
RsrCycleDetected class >> signal: anObject [

	^self new
		object: anObject;
		signal
]

{ #category : #accessing }
RsrCycleDetected >> messageText [

	^'Cycle detected on: ', object printString
]

{ #category : #accessing }
RsrCycleDetected >> object: anObject [

	object := anObject
]
