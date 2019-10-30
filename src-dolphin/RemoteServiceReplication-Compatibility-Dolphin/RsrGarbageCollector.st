RsrObject
	subclass: #RsrGarbageCollector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrGarbageCollector categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

!RsrGarbageCollector class methodsFor!
invokeGarbageCollector

	MemoryManager current collectGarbage!

maximumReclamation

	| service element sema didFinalize action |
	service := RsrAbstractService new.
	sema := Semaphore new.
	didFinalize := false.
	action :=
		[didFinalize := true.
		sema signal].
	element := RsrWeakRegistryElement
		value: service
		toFinalizeEvaluate: action.
	service := nil.
	self invokeGarbageCollector.
	[(Delay forSeconds: 1) wait. "Wait up to one second for finalization"
	sema signal] fork.
	sema wait.
	^didFinalize! !
