Class {
	#name : #RsrAbstractValueHolderService,
	#superclass : #RsrService,
	#instVars : [
		'value'
	],
	#category : #'RemoteServiceReplication-Test-Services'
}

{ #category : #accessing }
RsrAbstractValueHolderService class >> clientClass [

	^RsrValueHolderClient
]

{ #category : #accessing }
RsrAbstractValueHolderService class >> serverClass [

	^RsrValueHolderServer
]

{ #category : #'instance creation' }
RsrAbstractValueHolderService class >> value: anRsrObject [

	^self new
		value: anRsrObject;
		yourself
]

{ #category : #accessing }
RsrAbstractValueHolderService >> value [

	^value
]

{ #category : #accessing }
RsrAbstractValueHolderService >> value: anObject [

	value := anObject.
	self synchronize
]
