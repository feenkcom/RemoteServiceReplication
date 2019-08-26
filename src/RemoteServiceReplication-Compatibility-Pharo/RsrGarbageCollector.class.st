Class {
	#name : #RsrGarbageCollector,
	#superclass : #RsrObject,
	#category : #'RemoteServiceReplication-Compatibility-Pharo-RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #cleaning }
RsrGarbageCollector class >> maximumReclamation [

	| obj sema |
	obj := Object new.
	sema := Semaphore new.
	obj
		toFinalizeSend: #cull:
		to: [sema signal]
		with: nil.
	obj := nil.
	Smalltalk garbageCollect.
	Processor yield.
	^(sema waitTimeoutMSecs: 10) not
]
