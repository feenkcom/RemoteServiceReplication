Class {
	#name : 'RsrAbstractServiceFactory',
	#superclass : 'RsrService',
	#category : 'RemoteServiceReplication'
}

{ #category : 'accessing' }
RsrAbstractServiceFactory class >> clientClassName [

	^#RsrServiceFactory
]

{ #category : 'accessing' }
RsrAbstractServiceFactory class >> serverClassName [

	^#RsrServiceFactoryServer
]
