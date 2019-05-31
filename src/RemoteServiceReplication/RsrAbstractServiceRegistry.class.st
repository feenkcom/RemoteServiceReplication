Class {
	#name : #RsrAbstractServiceRegistry,
	#superclass : #RsrService,
	#category : 'RemoteServiceReplication-Public'
}

{ #category : #accessing }
RsrAbstractServiceRegistry class >> clientClass [

	^RsrServiceRegistry
]

{ #category : #accessing }
RsrAbstractServiceRegistry class >> serverClass [

	^RsrServiceRegistry
]
