Class {
	#name : 'RsrTestingProcessModel',
	#superclass : 'RsrProcessModel',
	#instVars : [
		'forkedException'
	],
	#category : 'RemoteServiceReplication-Platform-Test'
}

{ #category : 'managing-concurrency' }
RsrTestingProcessModel >> fork: aBlock [

	^super fork: (self protect: aBlock)
]

{ #category : 'managing-concurrency' }
RsrTestingProcessModel >> fork: aBlock
at: aPriority [

	^super
		fork: (self protect: aBlock)
		at: aPriority
]

{ #category : 'accessing' }
RsrTestingProcessModel >> forkedException [

	^forkedException
]

{ #category : 'accessing' }
RsrTestingProcessModel >> protect: aBlock [

	^[aBlock on: Exception do: [:ex | forkedException := ex copy. ex return]]
]
