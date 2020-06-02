Class {
	#name : 'RsrServiceFactory',
	#superclass : 'RsrService',
	#category : 'RemoteServiceReplication'
}

{ #category : 'accessing' }
RsrServiceFactory class >> clientClassName [

	^#RsrServiceFactoryClient
]

{ #category : 'accessing' }
RsrServiceFactory class >> serverClassName [

	^#RsrServiceFactoryServer
]

{ #category : 'accessing' }
RsrServiceFactory class >> templateClassName [

	^#RsrServiceFactory
]
