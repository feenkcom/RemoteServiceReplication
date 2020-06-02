Class {
	#name : 'RsrServiceFactoryServer',
	#superclass : 'RsrServiceFactory',
	#category : 'RemoteServiceReplication'
}

{ #category : 'mirroring' }
RsrServiceFactoryServer >> create: aResponsibility [

	| abstractClass |
	abstractClass := RsrClassResolver classNamed: aResponsibility.
	^abstractClass serverClass new
]
