Class {
	#name : #RsrServiceFactory,
	#superclass : #RsrAbstractServiceFactory,
	#category : #'RemoteServiceReplication-Public-Examples'
}

{ #category : #'public-manufacturing' }
RsrServiceFactory >> mirror: aService [

	remoteSelf return: aService
]

{ #category : #'public-manufacturing' }
RsrServiceFactory >> serviceFor: aResponsibility [

	| instance |
	instance := (RsrClassResolver classNamed: aResponsibility) basicNew.
	self mirror: instance.
	^instance
]
