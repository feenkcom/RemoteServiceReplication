Class {
	#name : 'RsrAbstractValueHolderService',
	#superclass : 'RsrService',
	#instVars : [
		'value'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrAbstractValueHolderService class >> clientClassName [

	^#RsrValueHolderClient
]

{ #category : 'accessing' }
RsrAbstractValueHolderService class >> serverClassName [

	^#RsrValueHolderServer
]

{ #category : 'instance creation' }
RsrAbstractValueHolderService class >> value: anRsrObject [

	^self new
		value: anRsrObject;
		yourself
]

{ #category : 'accessing' }
RsrAbstractValueHolderService >> value [

	^value
]

{ #category : 'accessing' }
RsrAbstractValueHolderService >> value: anObject [

	value := anObject.
	self synchronize
]
