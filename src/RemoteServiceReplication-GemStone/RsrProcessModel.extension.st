Extension { #name : 'RsrProcessModel' }

{ #category : '*remoteservicereplication-gemstone' }
RsrProcessModel >> currentStackDump [

	^GsProcess stackReportToLevel: 1000
]
