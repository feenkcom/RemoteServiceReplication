Class {
	#name : 'RsrProcessModel',
	#superclass : 'Object',
	#classInstVars : [
		'current'
	],
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'managing-concurrency' }
RsrProcessModel class >> currentStackDump [

	^self current currentStackDump
]

{ #category : 'managing-concurrency' }
RsrProcessModel class >> fork: aBlock
at: aPriority
named: aString [

	^self current
		fork: aBlock
		at: aPriority
		named: aString
]

{ #category : 'managing-concurrency' }
RsrProcessModel class >> fork: aBlock
named: aString [

	^self current fork: aBlock named: aString
]

{ #category : 'accessing' }
RsrProcessModel class >> OLDcurrent [

	^current ifNil: [self resetCurrent]
]

{ #category : 'accessing' }
RsrProcessModel class >> OLDcurrent: concurrency [

	current := concurrency
]

{ #category : 'accessing' }
RsrProcessModel class >> OLDresetCurrent [

	^current := self new
]

{ #category : 'managing-concurrency' }
RsrProcessModel class >> renameProcess: aString [
	"Rename the current process to the provided string"

	^self current renameProcess: aString
]

{ #category : 'managing-concurrency' }
RsrProcessModel >> fork: aBlock
at: aPriority
named: aString [

	[self renameProcess: aString.
	aBlock value] forkAt: aPriority
]

{ #category : 'managing-concurrency' }
RsrProcessModel >> fork: aBlock
named: aString [

	[self renameProcess: aString.
	aBlock value] fork
]

{ #category : 'renaming' }
RsrProcessModel >> renameProcess: aString [

	Processor activeProcess name: aString
]
