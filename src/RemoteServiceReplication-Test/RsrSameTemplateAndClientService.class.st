Class {
	#name : 'RsrSameTemplateAndClientService',
	#superclass : 'RsrService',
	#instVars : [
		'replicated1',
		'replicated2'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrSameTemplateAndClientService class >> clientClassName [

	^self templateClassName
]

{ #category : 'accessing' }
RsrSameTemplateAndClientService class >> serverClassName [

	^#RsrDifferentServerService
]

{ #category : 'accessing' }
RsrSameTemplateAndClientService class >> templateClassName [

	^#RsrSameTemplateAndClientService
]

{ #category : 'accessing' }
RsrSameTemplateAndClientService >> replicated1 [

	^replicated1
]

{ #category : 'accessing' }
RsrSameTemplateAndClientService >> replicated1: anObject [

	replicated1 := anObject
]

{ #category : 'accessing' }
RsrSameTemplateAndClientService >> replicated2 [

	^replicated2
]

{ #category : 'accessing' }
RsrSameTemplateAndClientService >> replicated2: anObject [

	replicated2 := anObject
]
