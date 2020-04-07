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
	self assert: RsrClientNoInstVars allInstances isEmpty.
	service := RsrServerNoInstVars new.
	service
		_id: 2
		connection: connectionA.
	command := RsrRetainObject object: service.
	command
		encodeUsing: connectionA encoder;
		writeUsing: connectionA commandWriter.
	connectionA commandWriter flush.
	(Delay forMilliseconds: 10) wait.
	connectionA close.
	connectionB close.
	service := command := nil.
	self maximumReclamation.
	self assert: RsrClientNoInstVars allInstances isEmpty
]

{ #category : #running }
RsrLifetimeTest >> testObjectCreatedViaServiceFactory [
	"Ensure objects created by the RsrServiceFactory maintain the same
	properties as other objects"

	| service remoteService |
	service := connectionA serviceFor: #RsrClientNoInstVars.
	self
		assert: (connectionA registry serviceAt: service _id)
		identicalTo: service.
	remoteService := connectionB registry serviceAt: service _id.
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
	valueServiceRemote := connectionB registry serviceAt: valueServiceLocal _id.
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
	actual := connectionA registry serviceAt: id ifAbsent: [marker].
	self
		assert: actual
		equals: marker.
	actual := connectionB registry serviceAt: id ifAbsent: [marker].
	self
		assert: actual
		equals: marker
]
