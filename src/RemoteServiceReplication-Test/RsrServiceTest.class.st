Class {
	#name : #RsrServiceTest,
	#superclass : #RsrSystemTestCase,
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #'running-utilities' }
RsrServiceTest >> mirror: aService [

	(connectionA serviceFor: #RsrClientNoInstVars) sendReturnArgument: aService
]

{ #category : #accessing }
RsrServiceTest >> newInstance [

	^RsrClientTestService new
]

{ #category : #running }
RsrServiceTest >> testHasRemoteSelf [

	| service |
	service := RsrClientTestService new.
	self mirror: service.
	self deny: nil == service remoteSelf
]

{ #category : #running }
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

{ #category : #running }
RsrServiceTest >> testIsMirrored [

	| instance |
	instance := RsrClientTestService new.
	self deny: instance isMirrored.
	self mirror: instance.
	self assert: instance isMirrored
]

{ #category : #running }
RsrServiceTest >> testMessagesDispatchedEvaluateConcurrently [
	"Ensure a long-running process in one Service will not cause startvation of other Services"

	| delayClient delayServer client server initialCounter counter delayedResult delaySem immediateResult |
	delayClient := RsrConcurrentTestClient new.
	client := RsrConcurrentTestClient new.
	self mirror: delayClient.
	self mirror: client.
	initialCounter := 0.
	counter := Association key: #counter value: initialCounter.
	delayServer := connectionB registry at: delayClient _id.
	server := connectionB registry at: client _id.
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
		equals: initialCounter + 1.
]

{ #category : #running }
RsrServiceTest >> testReflectedVariableNames [

	| clientNames serverNames |
	clientNames := RsrClientTestService _variablesToReflect.
	serverNames := RsrServerTestService _variablesToReflect.
	self
		assert: clientNames
		equals: serverNames.
	self
		assert: clientNames size
		equals: 1.
	self
		assert: (clientNames at: 1) asSymbol
		equals: #sharedVariable.
	clientNames := RsrReflectedVariableTestClient _variablesToReflect.
	serverNames := RsrReflectedVariableTestServer _variablesToReflect.
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

{ #category : #running }
RsrServiceTest >> testVariableReflection [

	| localService remoteService |
	localService := RsrClientTestService new
		sharedVariable: #shared;
		privateVariable: #private;
		yourself.
	self mirror: localService.
	remoteService := connectionB registry at: localService _id.
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
