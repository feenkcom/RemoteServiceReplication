Class {
	#name : 'RsrUnknownOID',
	#superclass : 'RsrError',
	#category : 'RemoteServiceReplication'
}

{ #category : 'signaling' }
RsrUnknownOID class >> signal [

	Transcript
		show: RsrProcessModel currentStackDump;
		cr.
	^super signal
]
