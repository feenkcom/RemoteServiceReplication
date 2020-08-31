"
No class-specific documentation for RsrValueReference, hierarchy is:
Object
  RsrObject
    RsrReference
      RsrImmediateReference
        RsrValueReference( value)

"
Class {
	#name : 'RsrValueReference',
	#superclass : 'RsrImmediateReference',
	#instVars : [
		'value'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrValueReference class >> from: anObject [

	^self value: anObject
]

{ #category : 'instance creation' }
RsrValueReference class >> value: anObject [

	^self new
		value: anObject;
		yourself
]

{ #category : 'resolving' }
RsrValueReference >> resolve: aRegistry [

	^value
]

{ #category : 'private-accessing' }
RsrValueReference >> value: anObject [

	value := anObject
]
