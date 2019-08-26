Class {
	#name : #RsrMessageSendingTest,
	#superclass : #RsrSystemTestCase,
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #running }
RsrMessageSendingTest >> testRemoteError [

	| service |
	service := connectionA serviceFor: #RsrEvaluationClient.
	self
		should: [service evaluate: 'Error signal']
		raise: Error
]

{ #category : #running }
RsrMessageSendingTest >> testReturnArgument [

	| client server arguments dt response seconds |
	client := RsrClientNoInstVars new.
	self sharedNamespaceA
		at: 'TestService'
		put: client.
	server := self sharedNamespaceB at: 'TestService'.
	arguments := OrderedCollection new
		addAll: #( #symbol 'string' $h 0 -14 14 18446744073709551616 -18446744073709551616 nil true false );
 		add: (Character codePoint: 16r259F);
		add: (Dictionary new at: 1 put: 2; yourself);
		add: (Set with: 14);
		add: #[1 2 3 4];
		add: (OrderedCollection with: 42 with: 43);
		add: #(1 2 #(nil));
		yourself.
	dt := RsrDateTimeInterface now.
	response := client sendReturnArgument: dt.
	self
		assert: (dt asSeconds * 1000000) rounded
		equals: (response asSeconds * 1000000) rounded.
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
		assert: (client sendReturnArgument: arguments)
		equals: arguments.
	self
		assert: (server sendReturnArgument: arguments)
		equals: arguments.
	self
		assert: (client sendReturnArgument: client)
		identicalTo: client.
	self
		assert: (server sendReturnArgument: server)
		identicalTo: server
]

{ #category : #running }
RsrMessageSendingTest >> testReturnInvalidObject [

	| service |
	service := connectionA serviceFor: #RsrClientNoInstVars.
	self
		should: [service sendReturnObjectNew]
		raise: RsrUnsupportedObject
]

{ #category : #running }
RsrMessageSendingTest >> testReturnNewService [

	| service returnedService |
	service := connectionA serviceFor: #RsrClientNoInstVars.
	returnedService := service sendReturnNewService.
	self
		assert: returnedService class
		equals: RsrClientTestService
]

{ #category : #running }
RsrMessageSendingTest >> testReturnNewServiceInArray [

	| service array returnedService |
	service := connectionA serviceFor: #RsrClientNoInstVars.
	array := service sendReturnNewServiceInArray.
	self
		assert: array size
		equals: 1.
	returnedService := array first.
	self
		assert: returnedService class
		equals: RsrClientTestService
]

{ #category : #running }
RsrMessageSendingTest >> testReturnSymbol [

	| service |
	service := connectionA serviceFor: #RsrClientNoInstVars.
	self
		assert: service sendReturnTestSymbol
		equals: #testSymbol
]

{ #category : #running }
RsrMessageSendingTest >> testSendInvalidObject [

	| service |
	service := connectionA serviceFor: #RsrClientNoInstVars.
	self
		should: [service sendReturnArgument: Object new]
		raise: RsrUnsupportedObject
]

{ #category : #running }
RsrMessageSendingTest >> testSendSetMarker [

	| service remoteService |
	service := RsrClientNoInstVars new.
	self sharedNamespaceA
		at: 'TestService'
		put: service.
	remoteService := self sharedNamespaceB at: 'TestService'.
	self deny: remoteService marker.
	service sendSetMarker.
	self assert: remoteService marker
]
