Class {
	#name : 'RsrLifetimeTest',
	#superclass : 'RsrSystemTestCase',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'testing' }
RsrLifetimeTest class >> isAbstract [

	^self == RsrLifetimeTest
]

{ #category : 'running' }
RsrLifetimeTest >> testObjectCreatedViaServiceFactory [
	"Ensure objects created by the RsrServiceFactory maintain the same
	properties as other objects"

	| client server remoteService |
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [:object | object].
	self
		assert: (connectionA serviceAt: client _id)
		identicalTo: client.
	remoteService := connectionB serviceAt: client _id.
	self
		assert: remoteService class
		equals: RsrRemoteActionServer.
	self
		assert: (client value: client)
		identicalTo: client
]

{ #category : 'running' }
RsrLifetimeTest >> testRemoteReferenceLifetime [

	| valueServiceLocal valueServiceRemote serviceLocal serviceRemote id marker actual |
	serviceLocal := RsrClientNoInstVars new.
	valueServiceLocal := connectionA serviceFor: #RsrValueHolderClient.
	valueServiceLocal value: serviceLocal.
	valueServiceRemote := connectionB serviceAt: valueServiceLocal _id.
	serviceRemote := valueServiceRemote value.
	id := serviceLocal _id.
	self
		assert: serviceRemote class
		equals: RsrServerNoInstVars.
	serviceLocal := serviceRemote := nil.
	valueServiceRemote value: nil.
	self maximumReclamation.
	(Delay forSeconds: 1) wait. "Needed to ensure there is time for release to propogate to remote environment."
	self maximumReclamation.
	marker := Object new.
	actual := connectionA serviceAt: id ifAbsent: [marker].
	self
		assert: actual
		equals: marker.
	actual := connectionB serviceAt: id ifAbsent: [marker].
	self
		assert: actual
		equals: marker
]
