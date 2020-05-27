Class {
	#name : 'RsrServiceFactory',
	#superclass : 'RsrAbstractServiceFactory',
	#category : 'RemoteServiceReplication'
}

{ #category : 'public-manufacturing' }
RsrServiceFactory >> serviceFor: aResponsibility [

	^remoteSelf create: aResponsibility
]
