Class {
	#name : 'RsrServiceFactoryServer',
	#superclass : 'RsrServiceFactory',
	#category : 'RemoteServiceReplication'
}

{ #category : 'manufactoring' }
RsrServiceFactoryServer >> create: aResponsibility [

	| abstractClass |
	abstractClass := RsrClassResolver classNamed: aResponsibility.
	^abstractClass serverClass new
]

{ #category : 'mirroring' }
RsrServiceFactoryServer >> mirror: aService [

	^aService
]
