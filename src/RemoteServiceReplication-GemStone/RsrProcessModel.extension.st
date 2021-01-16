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
RsrProcessModel >> currentStackDump [

	^GsProcess stackReportToLevel: 1000
]
