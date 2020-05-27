Class {
	#name : 'RsrSameAbstractAndClientService',
	#superclass : 'RsrService',
	#instVars : [
		'replicated1',
		'replicated2'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrSameAbstractAndClientService class >> abstractClassName [

	^#RsrSameAbstractAndClientService
]

{ #category : 'accessing' }
RsrSameAbstractAndClientService class >> clientClassName [

	^#RsrSameAbstractAndClientService
]

{ #category : 'accessing' }
RsrSameAbstractAndClientService class >> serverClassName [

	^#RsrDifferentServerService
]

{ #category : 'accessing' }
RsrSameAbstractAndClientService >> replicated1 [

	^replicated1
]

{ #category : 'accessing' }
RsrSameAbstractAndClientService >> replicated1: anObject [

	replicated1 := anObject
]

{ #category : 'accessing' }
RsrSameAbstractAndClientService >> replicated2 [

	^replicated2
]

{ #category : 'accessing' }
RsrSameAbstractAndClientService >> replicated2: anObject [

	replicated2 := anObject
]
