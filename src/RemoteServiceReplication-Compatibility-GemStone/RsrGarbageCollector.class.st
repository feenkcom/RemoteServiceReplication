Class {
	#name : #RsrGarbageCollector,
	#superclass : #RsrObject,
	#category : #'RemoteServiceReplication-Compatibility-GemStone'
}

{ #category : #cleaning }
RsrGarbageCollector class >> maximumReclamation [

	| object ephemeron sema |
	object := Object new.
	sema := Semaphore new.
	ephemeron := RsrEphemeron
		on: object
		mournAction: [sema signal].
	object := nil.
	System
		_generationScavenge_vmMarkSweep;
		_generationScavenge_vmMarkSweep.
	^sema waitForMilliseconds: 10
]
