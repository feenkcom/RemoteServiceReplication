Class {
	#name : 'RsrServiceFactoryServer',
	#superclass : 'RsrAbstractServiceFactory',
	#category : 'RemoteServiceReplication'
}

{ #category : 'mirroring' }
RsrServiceFactoryServer >> create: aResponsibility [

	| abstractClass |
	abstractClass := RsrClassResolver classNamed: aResponsibility.
	^abstractClass serverClass new
]
