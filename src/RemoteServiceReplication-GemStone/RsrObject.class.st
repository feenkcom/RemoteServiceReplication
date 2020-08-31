Class {
	#name : 'RsrObject',
	#superclass : 'Object',
	#category : 'RemoteServiceReplication-GemStone'
}

{ #category : 'reflection' }
RsrObject class >> allInstances [

	^self instancesInMemory
]

{ #category : 'instance creation' }
RsrObject class >> new [

	^super new initialize
]

{ #category : 'initialization' }
RsrObject >> initialize [

	^self
]

{ #category : 'errors' }
RsrObject >> subclassResponsibility [

	Transcript
		show: RsrProcessModel currentStackDump;
		cr.
	^super subclassResponsibility
]
