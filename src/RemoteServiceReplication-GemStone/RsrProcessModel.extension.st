Extension { #name : 'RsrProcessModel' }

{ #category : '*remoteservicereplication-gemstone' }
RsrProcessModel class >> current [
	^ SessionTemps current
		at: self keyForCurrent
		ifAbsent: [ self resetCurrent ]
]

{ #category : '*remoteservicereplication-gemstone' }
RsrProcessModel class >> current: concurrency [
	^ SessionTemps current at: self keyForCurrent put: concurrency
]

{ #category : '*remoteservicereplication-gemstone' }
RsrProcessModel class >> keyForCurrent [
	^ #'RsrCurrentProcessModel'
]

{ #category : '*remoteservicereplication-gemstone' }
RsrProcessModel class >> resetCurrent [
	^ self current: self new
]

{ #category : '*remoteservicereplication-gemstone' }
RsrProcessModel class >> unhandledExceptionClass [
	"Return the class which signals that an unhandled exception has been signaled."

	^RsrUnhandledException
]

{ #category : '*remoteservicereplication-gemstone' }
RsrProcessModel >> configureUnhandleExceptionProtection [

	Processor activeProcess
		breakpointLevel: 1;
		debugActionBlock:
			[:ex |
			({ClientForwarderSend. Halt. RsrUnhandledException.} includes: ex class)
				ifTrue: [ex _signalGciError]
				ifFalse: [RsrUnhandledException signal: ex]].
]

{ #category : '*remoteservicereplication-gemstone' }
RsrProcessModel >> currentStackDump [

	^GsProcess stackReportToLevel: 1000
]
