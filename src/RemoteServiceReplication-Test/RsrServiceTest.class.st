Class {
	#name : 'RsrServiceTest',
	#superclass : 'RsrSystemTestCase',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'running-utilities' }
RsrServiceTest >> mirror: aService [

	^(connectionA serviceFor: #RsrClientNoInstVars) sendReturnArgument: aService
]

{ #category : 'running' }
RsrServiceTest >> testCreateServiceWithDistinctClientAbstractService [

	| client |
	client := self serviceFactoryA serviceFor: #RsrTestService.
	self
		assert: client class
		equals: RsrClientTestService
]

{ #category : 'running' }
RsrServiceTest >> testCreateServiceWithSameClientAbstractService [

	| client server |
	client := self serviceFactoryA serviceFor: #RsrSameTemplateAndClientService.
	self
		assert: client class
		equals: RsrSameTemplateAndClientService.
	server := connectionB registry serviceAt: client _id.
	self
		assert: server replicated1
		equals: nil.
	self
		assert: server replicated2
		equals: nil.
	client
		replicated1: 1;
		replicated2: 2;
		synchronize.
	self
		assert: server replicated1
		equals: 1.
	self
		assert: server replicated2
		equals: 2.
	server
		replicated1: 10;
		replicated2: 20;
		private1: 3;
		synchronize.
	self
		assert: client replicated1
		equals: 10.
	self
		assert: client replicated2
		equals: 20
]

{ #category : 'running' }
RsrServiceTest >> testEnsureServersAreCachedAndReused [

	| client service1 service2 |
	client := RsrClientTestService new.
	self mirror: client.
	service1 := connectionB registry serviceAt: client _id.
	self mirror: client.
	service2 := connectionB registry serviceAt: client _id.
	self
		assert: service1
		identicalTo: service2
]

{ #category : 'running' }
RsrServiceTest >> testHasRemoteSelf [

	| service |
	service := RsrClientTestService new.
	self mirror: service.
	self deny: nil == service remoteSelf
]

{ #category : 'running' }
RsrServiceTest >> testInitialization [

	| instance |
	instance := RsrClientTestService new.
	self
		assert: instance isMirrored
		equals: false.
	self
		assert: instance _id
		equals: nil.
	self
		assert: instance _connection
		equals: nil
]

{ #category : 'running' }
RsrServiceTest >> testIsMirrored [

	| instance |
	instance := RsrClientTestService new.
	self deny: instance isMirrored.
	self mirror: instance.
	self assert: instance isMirrored
]

{ #category : 'running' }
RsrServiceTest >> testMessageDispatchedSeriallyAndToSameProcessForSingleService [
	"Ensure that when a message is sent to a Service it is always dispatched to the same process"

	| client server process1 process2 |
	client := self mirror: RsrConcurrentTestClient new.
	server := connectionB registry serviceAt: client _id.
	client stashProcess.
	process1 := server stashedProcess.
	client stashProcess.
	process2 := server stashedProcess.
	self
		assert: process1
		identicalTo: process2
]

{ #category : 'running' }
RsrServiceTest >> testMessagesDispatchedSeriallyForMultipleServices [
	"Ensure a long-running process in one Service will not cause startvation of other Services"

	| delayClient delayServer client server initialCounter counter delayedResult delaySem immediateResult |
	delayClient := RsrConcurrentTestClient new.
	client := RsrConcurrentTestClient new.
	self mirror: delayClient.
	self mirror: client.
	initialCounter := 0.
	counter := Array with: initialCounter.
	delayServer := connectionB registry serviceAt: delayClient _id.
	server := connectionB registry serviceAt: client _id.
	delaySem := Semaphore new.
	delayServer
		counter: counter;
		semaphore: delaySem.
	server counter: counter.
	self
		assumption: 'The call to #delayedCounter needs to run before the methods continues.';
		assumption: 'The delay period in #delayedCounter will be enough to ensure #counterWithIncrement processes'.
	RsrConcurrency fork: [delayedResult := delayClient delayedCounter. delaySem signal].
	delaySem wait.
	immediateResult := client counterWithIncrement.
	self
		assert: immediateResult
		equals: initialCounter.
	delaySem wait.
	self
		assert: delayedResult
		equals: initialCounter
]

{ #category : 'running' }
RsrServiceTest >> testReflectedVariableNames [

	| client server clientNames serverNames |
	client := connectionA serviceFor: #RsrTestService.
	server := connectionB registry serviceAt: client _id.
	clientNames := client reflectedVariableNames.
	serverNames := server reflectedVariableNames.
	self
		assert: clientNames
		equals: serverNames.
	self
		assert: clientNames size
		equals: 1.
	self
		assert: (clientNames at: 1) asSymbol
		equals: #sharedVariable.
	client := connectionA serviceFor: #RsrReflectedVariableTestServiceB.
	server := connectionB registry serviceAt: client _id.
	clientNames := client reflectedVariableNames.
	serverNames := server reflectedVariableNames.
	self
		assert: clientNames
		equals: serverNames.
	self
		assert: clientNames size
		equals: 2.
	self
		assert: (clientNames at: 1) asSymbol
		equals: #varA.
	self
		assert: (clientNames at: 2) asSymbol
		equals: #varB
]

{ #category : 'running' }
RsrServiceTest >> testVariableReflection [

	| localService remoteService |
	localService := RsrClientTestService new
		sharedVariable: #shared;
		privateVariable: #private;
		yourself.
	self mirror: localService.
	remoteService := connectionB registry serviceAt: localService _id.
	self
		assert: localService sharedVariable
		identicalTo: remoteService sharedVariable.
	self
		assert: localService privateVariable
		identicalTo: #private.
	self
		assert: remoteService privateVariable
		identicalTo: nil
]
