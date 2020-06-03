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

	^remoteSelf create: aResponsibility
]
