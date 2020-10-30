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

{ #category : 'notes' }
RsrObject >> note: aString [
	"This method can be used to leave a note in code. For instance, a code path that needs to be tested."
]

{ #category : 'tracing' }
RsrObject >> trace [

	Transcript
		show: RsrProcessModel currentStackDump;
		cr;
		cr
]
