Class {
	#name : 'RsrValueReference',
	#superclass : 'RsrReference',
	#instVars : [
		'value'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrValueReference class >> value: anObject [

	^self new
		value: anObject;
		yourself
]

{ #category : 'resolving' }
RsrValueReference >> resolve: aRegistry [

	^self value
]

{ #category : 'accessing' }
RsrValueReference >> value [

	^value
]

{ #category : 'accessing' }
RsrValueReference >> value: anObject [

	value := anObject
]
