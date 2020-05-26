Class {
	#name : 'RsrServiceFactory',
	#superclass : 'RsrAbstractServiceFactory',
	#category : 'RemoteServiceReplication'
}

{ #category : 'public-manufacturing' }
RsrServiceFactory >> mirror: aService [

	remoteSelf return: aService
]

{ #category : 'public-manufacturing' }
RsrServiceFactory >> serviceFor: aResponsibility [

	| abstractClass instance |
	abstractClass := RsrClassResolver classNamed: aResponsibility.
	instance := abstractClass clientClass new.
	self mirror: instance.
	^instance
]
