Class {
	#name : 'RsrMessageSendingTest',
	#superclass : 'RsrSystemTestCase',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'running-errors' }
RsrMessageSendingTest >> testRemoteError [

	| client server exception |
	client := connectionA serviceFor: #RsrRemoteAction.
	server := connectionB registry serviceAt: client _id.
	server action: [Error new tag: 'tag'; messageText: 'messageText'; signal].
	[client value]
		on: RsrRemoteError
		do: [:ex | exception := ex. ex return].
	self
		assert: exception class
		equals: RsrRemoteError.
	self
		assert: exception originalClassName
		equals: #Error.
	self
		assert: exception tag
		equals: 'tag'.
	self
		assert: exception messageText
		equals: 'messageText'.
	self
		assert: exception stack isString;
		assert: exception stack size > 0
]

{ #category : 'running' }
RsrMessageSendingTest >> testReturnAlsoUpdatesLocalService [
	"Ensure that when the remote peer service returns a value,
	that it is also sent to update the local service."

	| client server value response |
	client := self serviceFactoryA serviceFor: #RsrReflectedVariableTestServiceB.
	server := connectionB registry serviceAt: client _id.
	value := 42.
	self
		deny: client varA
		equals: value.
	self
		deny: client varB
		equals: value.
	response := client setVarsToAndReturn: value.
	self
		assert: response
		equals: value.
	self
		assert: server varA
		equals: value.
	self
		assert: server varB
		equals: value.
	self
		assert: client varA
		equals: value.
	self
		assert: client varB
		equals: value
]

{ #category : 'running' }
RsrMessageSendingTest >> testReturnArgument [

	| client server arguments dt response |
	client := connectionA serviceFor: #RsrClientNoInstVars.
	server := connectionB registry serviceAt: client _id.
	arguments := OrderedCollection new
		addAll: #( '' #symbol 'string' $h 0 -14 14 18446744073709551616 -18446744073709551616 nil true false );
 		add: (Character codePoint: 16r259F);
		add: (Dictionary new at: 1 put: 2; yourself);
		add: (Set with: 14);
		add: #[1 2 3 4];
		add: (OrderedCollection with: 42 with: 43);
		add: #(1 2 #(nil));
		yourself.
	dt := RsrDateAndTimeSpecies now.
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

{ #category : 'running-errors' }
RsrMessageSendingTest >> testReturnInvalidObject [

	| client server exception |			
	client := connectionA serviceFor: #RsrRemoteAction.
	server := connectionB registry serviceAt: client _id.
	server action: [Object new].
	self
		should: [client value]
		raise: RsrRemoteError.
	[client value]
		on: RsrRemoteError
		do: [:ex | exception := ex. ex return].
	self
		assert: exception class
		equals: RsrRemoteError.
	self
		assert: exception originalClassName
		equals: #RsrUnsupportedObject.
	self
		assert: exception tag
		equals: 'Instances of Object cannot be serialized'.
	self
		assert: exception messageText
		equals: 'Instances of Object cannot be serialized'.
	self
		assert: exception stack isString;
		assert: exception stack size > 0
]

{ #category : 'running' }
RsrMessageSendingTest >> testReturnNewService [

	| service returnedService |
	service := connectionA serviceFor: #RsrClientNoInstVars.
	returnedService := service sendReturnNewService.
	self
		assert: returnedService class
		equals: RsrClientTestService
]

{ #category : 'running' }
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

{ #category : 'running' }
RsrMessageSendingTest >> testReturnSymbol [

	| service |
	service := connectionA serviceFor: #RsrClientNoInstVars.
	self
		assert: service sendReturnTestSymbol
		equals: #testSymbol
]

{ #category : 'running-errors' }
RsrMessageSendingTest >> testSendInvalidObject [

	| client server |			
	client := connectionA serviceFor: #RsrRemoteAction.
	server := connectionB registry serviceAt: client _id.
	server action: [:arg | arg].
	self
		should: [client value: Object new]
		raise: RsrUnsupportedObject
]

{ #category : 'running' }
RsrMessageSendingTest >> testSendSetMarker [

	| service remoteService |
	service := connectionA serviceFor: #RsrClientNoInstVars.
	remoteService := connectionB registry serviceAt: service _id.
	self deny: remoteService marker.
	service sendSetMarker.
	self assert: remoteService marker
]
