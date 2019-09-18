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
RsrServiceTest >> testReflectedVariableNames [

	| clientNames serverNames |
	clientNames := RsrReflection reflectedVariableNamesOf: RsrClientTestService new.
	serverNames := RsrReflection reflectedVariableNamesOf: RsrServerTestService new.
	self
		assert: clientNames
		equals: serverNames.
	self
		assert: clientNames size
		equals: 1.
	self
		assert: (clientNames at: 1) asSymbol
		equals: #sharedVariable.
	clientNames := RsrReflection reflectedVariableNamesOf: RsrReflectedVariableTestClient new.
	serverNames := RsrReflection reflectedVariableNamesOf: RsrReflectedVariableTestServer new.
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
