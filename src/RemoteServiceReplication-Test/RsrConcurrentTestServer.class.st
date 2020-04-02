Class {
	#name : #RsrConcurrentTestServer,
	#superclass : #RsrAbstractConcurrentTestService,
	#instVars : [
		'counter',
		'semaphore'
	],
	#category : #'RemoteServiceReplication-Test-Services'
}

{ #category : #accessing }
RsrConcurrentTestServer class >> initialCounter [

	^0
]

{ #category : #accessing }
RsrConcurrentTestServer >> counter: anAssoc [

	counter := anAssoc
]

{ #category : #accessing }
RsrConcurrentTestServer >> counterWithIncrement [

	^[counter value] ensure: [counter value: counter value + 1]
]

{ #category : #accessing }
RsrConcurrentTestServer >> delayedCounter [

	semaphore signal.
	(Delay forSeconds: 2) wait.
	^counter value
]

{ #category : #accessing }
RsrConcurrentTestServer >> semaphore: aSemaphore [

	semaphore := aSemaphore
]
