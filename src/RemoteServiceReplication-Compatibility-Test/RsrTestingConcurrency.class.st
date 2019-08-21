Class {
	#name : #RsrTestingConcurrency,
	#superclass : #RsrConcurrency,
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
RsrTestingConcurrency >> protect: aBlock [

	^[aBlock on: Exception do: [:ex | "NOP"]]
]
