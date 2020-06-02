Class {
	#name : 'RsrServiceFactoryClient',
	#superclass : 'RsrServiceFactory',
	#category : 'RemoteServiceReplication'
}

{ #category : 'public-manufacturing' }
RsrServiceFactoryClient >> serviceFor: aResponsibility [

	^remoteSelf create: aResponsibility
]
