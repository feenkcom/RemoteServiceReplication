utilities
maximumReclamation

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
	self deny: (sema waitTimeoutMSecs: 100).
	