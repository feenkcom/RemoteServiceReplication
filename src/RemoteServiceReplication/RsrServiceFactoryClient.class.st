Class {
	#name : 'RsrServiceFactoryClient',
	#superclass : 'RsrServiceFactory',
	#category : 'RemoteServiceReplication'
}

{ #category : 'mirroring' }
RsrServiceFactoryClient >> mirror: aService [

	^remoteSelf mirror: aService
]

{ #category : 'manufactoring' }
RsrServiceFactoryClient >> serviceFor: aResponsibility [

	| abstractClass instance |
	abstractClass := RsrClassResolver classNamed: aResponsibility.
	instance := abstractClass clientClass new.
	instance registerWith: _connection.
	^instance
]
