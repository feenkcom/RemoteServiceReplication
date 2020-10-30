Class {
	#name : 'RsrObject',
	#superclass : 'Object',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'tracing' }
RsrObject class >> trace [

	Transcript
		show: RsrProcessModel currentStackDump;
		cr;
		cr
]

{ #category : 'tracing' }
RsrObject >> trace [

	Transcript
		show: RsrProcessModel currentStackDump;
		cr;
		cr
]
