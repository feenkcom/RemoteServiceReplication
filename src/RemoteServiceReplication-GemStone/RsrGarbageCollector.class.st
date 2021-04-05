Class {
	#name : 'RsrGarbageCollector',
	#superclass : 'RsrObject',
	#category : 'RemoteServiceReplication-GemStone'
}

{ #category : 'cleaning' }
RsrGarbageCollector class >> maximumReclamation [

	| sema ephemeron |
	sema := Semaphore new.
	ephemeron := RsrEphemeron
		on: Object new
		mournAction: [sema signal].
	System
		_generationScavenge_vmMarkSweep;
		_generationScavenge_vmMarkSweep.
	^sema waitForMilliseconds: 10
]
