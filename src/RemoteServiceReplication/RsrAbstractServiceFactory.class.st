Class {
	#name : #RsrAbstractServiceFactory,
	#superclass : #RsrService,
	#category : #'RemoteServiceReplication-Public-Examples'
}

{ #category : #accessing }
RsrAbstractServiceFactory class >> clientClassName [

	^#RsrServiceFactory
]

{ #category : #accessing }
RsrAbstractServiceFactory class >> serverClassName [

	^#RsrServiceFactoryServer
]
