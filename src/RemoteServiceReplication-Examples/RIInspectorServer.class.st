Class {
	#name : #RIInspectorServer,
	#superclass : #RIAbstractInspector,
	#instVars : [
		'object'
	],
	#category : 'RemoteServiceReplication-Examples-Inspector'
}

{ #category : #'instance creation' }
RIInspectorServer class >> object: anObject [

	^self new
		object: anObject;
		yourself
]

{ #category : #accessing }
RIInspectorServer >> object [

	^object
]

{ #category : #accessing }
RIInspectorServer >> object: anObject [

	object := anObject
]

{ #category : #'inspection protocol' }
RIInspectorServer >> objectPrintString [

	^object printString
]
