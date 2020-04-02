Class {
	#name : #RsrConcurrentTestClient,
	#superclass : #RsrAbstractConcurrentTestService,
	#category : #'RemoteServiceReplication-Test-Services'
}

{ #category : #accessing }
RsrConcurrentTestClient >> counterWithIncrement [

	^remoteSelf counterWithIncrement
]

{ #category : #accessing }
RsrConcurrentTestClient >> delayedCounter [

	^remoteSelf delayedCounter
]

{ #category : #actions }
RsrConcurrentTestClient >> stashProcess [

	remoteSelf stashProcess
]
