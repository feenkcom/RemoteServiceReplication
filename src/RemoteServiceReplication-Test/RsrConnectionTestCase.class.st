Class {
	#name : 'RsrConnectionTestCase',
	#superclass : 'RsrSystemTestCase',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'running' }
RsrConnectionTestCase >> testWaitUntilClose [

	| semaphore marker |
	semaphore := Semaphore new.
	marker := false.
	self
		fork:
			[semaphore signal.
			[connectionB waitUntilClose.
			marker := true]
				ensure: [semaphore signal]].
	semaphore wait.
	self deny: marker.
	connectionA close.
	semaphore wait.
	self assert: marker
]
