Class {
	#name : #RsrSendServiceTest,
	#superclass : #RsrSystemTestCase,
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #running }
RsrSendServiceTest >> testCloseWithDanglingObject [
	"It is possible that the connection could disconnect between when an object
	is received and when the upcoming SendMessage or DeliverResponse message is received.
	If this is the case, we coul leak memory due to the caching used to ensure
	the object is stored in memory long enough to process the upcoming message.
	Test to ensure the object is freed on the connection close."

	| service command |
	self maximumReclamation.
	self assert: RsrClientNoInstVars allInstances isEmpty.
	service := RsrServerNoInstVars new.
	service
		rsrId: 2;
		rsrConnection: connectionA.
	command := RsrRetainObject object: service.
	command
		encodeUsing: connectionA encoder;
		writeUsing: connectionA commandWriter.
	connectionA commandWriter flush.
	(Delay forSeconds: 1) wait.
	connectionA close.
	connectionB close.
	service := command := nil.
	self maximumReclamation.
	self assert: RsrClientNoInstVars allInstances isEmpty
]

{ #category : #running }
RsrSendServiceTest >> testRemoteReferenceLifetime [

	| name valueServiceLocal valueServiceRemote serviceLocal serviceRemote id marker actual |
	name := 'ValueService'.
	serviceLocal := RsrClientNoInstVars new.
	valueServiceLocal := RsrValueHolderClient value: serviceLocal.
	self sharedNamespaceA
		at: name
		put: valueServiceLocal.
	valueServiceRemote := self sharedNamespaceB at: name.
	serviceRemote := valueServiceRemote value.
	id := serviceLocal rsrId.
	self
		assert: serviceRemote class
		equals: RsrServerNoInstVars.
	serviceLocal := serviceRemote := nil.
	valueServiceRemote value: nil.
	self maximumReclamation.
	(Delay forSeconds: 2) wait.
	Smalltalk at: #KeepItAlive put: (Array with: connectionA with: connectionB).
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

{ #category : #running }
RsrSendServiceTest >> testReturnArgument [

	| client server arguments |
	client := RsrClientNoInstVars new.
	self sharedNamespaceA
		at: 'TestService'
		put: client.
	server := self sharedNamespaceB at: 'TestService'.
	arguments := OrderedCollection new
		addAll: #( #symbol 'string' $h 0 -14 14 18446744073709551616 -18446744073709551616 nil true false );
 		add: (Character codePoint: 16r259F);
		yourself.
	arguments
		do:
			[:each | | result |
			result := client sendReturnArgument: each.
			self
				assert: result
				equals: each].
	arguments
		do:
			[:each | | result |
			result := server sendReturnArgument: each.
			self
				assert: result
				equals: each].
	self
		assert: (client sendReturnArgument: client)
		identicalTo: client.
	self
		assert: (server sendReturnArgument: server)
		identicalTo: server
]

{ #category : #running }
RsrSendServiceTest >> testReturnSymbol [

	| service |
	service := connectionA serviceFor: #RsrClientNoInstVars.
	"service := RsrClientNoInstVars new.
	self serviceRegistryA
		at: 'TestService'
		put: service."
	self
		assert: service sendReturnTestSymbol
		equals: #testSymbol
]

{ #category : #running }
RsrSendServiceTest >> testSendSetMarker [

	| service remoteService |
	service := RsrClientNoInstVars new.
	self sharedNamespaceA
		at: 'TestService'
		put: service.
	remoteService := self sharedNamespaceB at: 'TestService'.
	service sendSetMarker.
	self assert: remoteService marker
]
