Class {
	#name : #RsrServiceFactory,
	#superclass : #RsrAbstractServiceFactory,
	#category : #'RemoteServiceReplication-Public-Examples'
}

{ #category : #'public-manufacturing' }
RsrServiceFactory >> serviceFor: aResponsibility [

	| instance |
	instance := (RsrClassLocator classNamed: aResponsibility)
		rsrId: rsrConnection oidSpigot next
		rsrConnection: rsrConnection.
	instance synchronize.
	^instance
]
