Class {
	#name : #RsrLifetimeTest,
	#superclass : #RsrSystemTestCase,
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #running }
RsrLifetimeTest >> testCloseWithDanglingObject [
	"It is possible that the connection could disconnect between when an object
	is received and when the upcoming SendMessage or DeliverResponse message is received.
	If this is the case, we could leak memory due to the caching used to ensure
	the object is stored in memory long enough to process the upcoming message.
	Test to ensure the object is freed on the connection close."

	| service command |
	self maximumReclamation.
	self assert: (RsrReflection allInstancesOf: RsrClientNoInstVars) isEmpty.
	service := RsrServerNoInstVars new.
	service
		_id: 2;
		_connection: connectionA.
	command := RsrRetainObject object: service.
	command
		encodeUsing: connectionA encoder;
		writeUsing: connectionA commandWriter.
	connectionA commandWriter flush.
	connectionA close.
	connectionB close.
	service := command := nil.
	self maximumReclamation.
	self assert: (RsrReflection allInstancesOf: RsrClientNoInstVars) isEmpty
]

{ #category : #running }
RsrLifetimeTest >> testObjectCreatedViaServiceFactor [
	"Ensure objects created by the RsrServiceFactory maintain the same
	properties as other objects"

	| service remoteService |
	service := connectionA serviceFor: #RsrClientNoInstVars.
	self
		assert: (connectionA registry at: service _id)
		identicalTo: service.
	remoteService := connectionB registry at: service _id.
	self
		assert: remoteService class
		equals: RsrServerNoInstVars.
	self
		assert: service sendReturnSelf
		identicalTo: service
]

{ #category : #running }
RsrLifetimeTest >> testRemoteReferenceLifetime [

	| valueServiceLocal valueServiceRemote serviceLocal serviceRemote id marker actual |
	serviceLocal := RsrClientNoInstVars new.
	valueServiceLocal := connectionA serviceFor: #RsrValueHolderClient.
	valueServiceLocal value: serviceLocal.
	valueServiceRemote := connectionB registry at: valueServiceLocal _id.
	serviceRemote := valueServiceRemote value.
	id := serviceLocal _id.
	self
		assert: serviceRemote class
		equals: RsrServerNoInstVars.
	serviceLocal := serviceRemote := nil.
	valueServiceRemote value: nil.
	self maximumReclamation.
	(Delay forSeconds: 1) wait. "Needed to ensure there is time for release to propogate to remote environment."
	marker := Object new.
	actual := connectionA registry at: id ifAbsent: [marker].
	self
		assert: actual
		equals: marker.
	actual := connectionB registry at: id ifAbsent: [marker].
	self
		assert: actual
		equals: marker
]
