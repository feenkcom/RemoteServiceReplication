Class {
	#name : #RsrTranscriptSink,
	#superclass : #RsrLogSink,
	#category : 'RemoteServiceReplication'
}

{ #category : #writing }
RsrTranscriptSink >> write: aMessageString [

	Transcript
		show: aMessageString;
		cr
]
