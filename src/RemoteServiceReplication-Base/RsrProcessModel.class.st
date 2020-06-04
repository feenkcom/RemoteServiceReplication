Class {
	#name : 'RsrProcessModel',
	#superclass : 'Object',
	#classInstVars : [
		'current'
	],
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'accessing' }
RsrProcessModel class >> current [

	^current ifNil: [self resetCurrent]
]

{ #category : 'accessing' }
RsrProcessModel class >> current: concurrency [

	current := concurrency
]

{ #category : 'managing-concurrency' }
RsrProcessModel class >> currentStackDump [

	^self current currentStackDump
]

{ #category : 'managing-concurrency' }
RsrProcessModel class >> fork: aBlock [

	^self current fork: aBlock
]

{ #category : 'managing-concurrency' }
RsrProcessModel class >> fork: aBlock
at: aPriority [

	^self current
		fork: aBlock
		at: aPriority
]

{ #category : 'accessing' }
RsrProcessModel class >> resetCurrent [

	^current := self new
]

{ #category : 'managing-concurrency' }
RsrProcessModel >> fork: aBlock [

	^aBlock fork
]

{ #category : 'managing-concurrency' }
RsrProcessModel >> fork: aBlock
at: aPriority [

	^aBlock forkAt: aPriority
]
