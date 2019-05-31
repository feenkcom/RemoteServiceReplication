Class {
	#name : #RsrServiceFactory,
	#superclass : #RsrAbstractServiceFactory,
	#category : #'RemoteServiceReplication-Public'
}

{ #category : #'public-manufacturing' }
RsrServiceFactory >> serviceFor: aResponsibility [

	| instance |
	instance := (Smalltalk at: aResponsibility)
		rsrId: rsrConnection oidSpigot next
		rsrConnection: rsrConnection.
	instance synchronize.
	^instance
]
