Class {
	#name : #RsrAbstractServiceFactory,
	#superclass : #RsrService,
	#category : #'RemoteServiceReplication-Public-Examples'
}

{ #category : #accessing }
RsrAbstractServiceFactory class >> clientClass [

	^RsrServiceFactory
]

{ #category : #accessing }
RsrAbstractServiceFactory class >> serverClass [

	^RsrServiceFactoryServer
]
