Extension { #name : 'RsrProcessModel' }

{ #category : '*remoteservicereplication-compatibility-gemstone' }
RsrProcessModel >> currentStackDump [

	^GsProcess stackReportToLevel: 1000
]
