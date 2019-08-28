Class {
	#name : #RsrRegistryElement,
	#superclass : #RsrObject,
	#instVars : [
		'value'
	],
	#category : #'RemoteServiceReplication-Compatibility-GemStone'
}

{ #category : #'instance-creation' }
RsrRegistryElement class >> with: anObject [

	^self new
        value: anObject;
        yourself
]

{ #category : #accessing }
RsrRegistryElement >> value [

	^value
]

{ #category : #accessing }
RsrRegistryElement >> value: anObject [

	value := anObject
]
