Class {
	#name : #RsrAbstractServiceFactory,
	#superclass : #RsrService,
	#category : #'RemoteServiceReplication-Public'
}

{ #category : #accessing }
RsrAbstractServiceFactory class >> clientClass [

	^RsrServiceFactory
]

{ #category : #accessing }
RsrAbstractServiceFactory class >> serverClass [

	^RsrServiceFactoryServer
]
