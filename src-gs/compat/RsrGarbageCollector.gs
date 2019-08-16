run
Object
	subclass: #RsrGarbageCollector
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

set class RsrGarbageCollector

classmethod:
maximumReclamation

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
%