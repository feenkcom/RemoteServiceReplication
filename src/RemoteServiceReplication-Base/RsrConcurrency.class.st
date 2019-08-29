Class {
	#name : #RsrConcurrency,
	#superclass : #Object,
	#classInstVars : [
		'current'
	],
	#category : #'RemoteServiceReplication-Base-Concurrency'
}

{ #category : #accessing }
RsrConcurrency class >> current [

	^current ifNil: [self resetCurrent]
]

{ #category : #accessing }
RsrConcurrency class >> current: concurrency [

	current := concurrency
]

{ #category : #'managing-concurrency' }
RsrConcurrency class >> fork: aBlock [

	^self current fork: aBlock
]

{ #category : #'managing-concurrency' }
RsrConcurrency class >> fork: aBlock
at: aPriority [

	^self current
		fork: aBlock
		at: aPriority
]

{ #category : #accessing }
RsrConcurrency class >> resetCurrent [

	^current := self new
]

{ #category : #'managing-concurrency' }
RsrConcurrency >> fork: aBlock [

	^aBlock fork
]

{ #category : #'managing-concurrency' }
RsrConcurrency >> fork: aBlock
at: aPriority [

	^aBlock forkAt: aPriority
]
