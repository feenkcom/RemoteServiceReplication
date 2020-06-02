Class {
	#name : 'RsrConcurrentTestClient',
	#superclass : 'RsrConcurrentTestService',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrConcurrentTestClient >> counterWithIncrement [

	^remoteSelf counterWithIncrement
]

{ #category : 'accessing' }
RsrConcurrentTestClient >> delayedCounter [

	^remoteSelf delayedCounter
]

{ #category : 'actions' }
RsrConcurrentTestClient >> stashProcess [

	remoteSelf stashProcess
]
