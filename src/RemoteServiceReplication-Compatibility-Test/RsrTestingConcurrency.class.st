Class {
	#name : #RsrTestingConcurrency,
	#superclass : #RsrConcurrency,
	#instVars : [
		'forkedException'
	],
	#category : #'RemoteServiceReplication-Compatibility-Test'
}

{ #category : #'managing-concurrency' }
RsrTestingConcurrency >> fork: aBlock [

	^super fork: (self protect: aBlock)
]

{ #category : #'managing-concurrency' }
RsrTestingConcurrency >> fork: aBlock
at: aPriority [

	^super
		fork: (self protect: aBlock)
		at: aPriority
]

{ #category : #accessing }
RsrTestingConcurrency >> forkedException [

	^forkedException
]

{ #category : #accessing }
RsrTestingConcurrency >> protect: aBlock [

	^[aBlock on: Exception do: [:ex | forkedException := ex copy. ex return]]
]
