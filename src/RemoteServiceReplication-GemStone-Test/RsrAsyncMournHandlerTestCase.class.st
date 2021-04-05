Class {
	#name : 'RsrAsyncMournHandlerTestCase',
	#superclass : 'RsrTestCase',
	#instVars : [
		'handler'
	],
	#category : 'RemoteServiceReplication-GemStone-Test'
}

{ #category : 'utilities' }
RsrAsyncMournHandlerTestCase >> finalizationProcess [

	| proc |
	self finalizeEphemeronWithAction: [proc := Processor activeProcess].
	^proc
]

{ #category : 'utilities' }
RsrAsyncMournHandlerTestCase >> finalizeEphemeronWithAction: aBlock [

	| semaphore ephemeron |
	semaphore := Semaphore new.
	ephemeron := RsrEphemeron
		on: Object new
		mournAction:
			[aBlock value.
			semaphore signal].
	RsrGarbageCollector maximumReclamation.
	self assert: (semaphore waitForSeconds: 1)
]

{ #category : 'initializing' }
RsrAsyncMournHandlerTestCase >> setUp [

	super setUp.
	handler := RsrAsyncMournHandler current.
	handler start
]

{ #category : 'initializing' }
RsrAsyncMournHandlerTestCase >> tearDown [

	handler stop.
	super tearDown
]

{ #category : 'running' }
RsrAsyncMournHandlerTestCase >> testAnotherAsyncHandlerExists [

	| notifier |
	handler stop.
	[notifier := GsSignalingSocket newForAsyncExceptions: {GcFinalizeNotification }.
	self
		should: [handler start]
		raise: RsrNotification] ensure: [GsSignalingSocket disableAsyncExceptions].
	self
		shouldnt: [handler start]
		raise: RsrNotification
]

{ #category : 'running' }
RsrAsyncMournHandlerTestCase >> testEnsureSingletonWhileActive [

	| duplicateHandler |
	duplicateHandler := RsrAsyncMournHandler current.
	self
		assert: duplicateHandler
		identicalTo: handler
]

{ #category : 'running' }
RsrAsyncMournHandlerTestCase >> testEnsureStarted [

	| nonHandlerFinalizeProcess handlerFinalizeProcess1 handlerFinalizeProcess2 |
	handler stop.
	nonHandlerFinalizeProcess := self finalizationProcess.
	self deny: handler isActive.
	handler ensureStarted.
	self assert: handler isActive.
	handlerFinalizeProcess1 := self finalizationProcess.
	handler ensureStarted.
	self assert: handler isActive.
	handlerFinalizeProcess2 := self finalizationProcess.
	self
		deny: nonHandlerFinalizeProcess
		identicalTo: handlerFinalizeProcess1.
	self
		assert: handlerFinalizeProcess1
		identicalTo: handlerFinalizeProcess2
]

{ #category : 'running' }
RsrAsyncMournHandlerTestCase >> testHandlerStops [

	| handlerProcess mournProcess |
	handlerProcess := handler process.
	handler stop.
	mournProcess := self finalizationProcess.
	self
		deny: mournProcess
		equals: handlerProcess.
	self deny: handlerProcess isNil
]

{ #category : 'running' }
RsrAsyncMournHandlerTestCase >> testMournEphemeron [

	| mournProcess |
	mournProcess := self finalizationProcess.
	self
		assert: mournProcess
		equals: handler process
]

{ #category : 'running' }
RsrAsyncMournHandlerTestCase >> testNewRaisesError [

	self
		should: [RsrAsyncMournHandler new]
		raise: RsrError
]
