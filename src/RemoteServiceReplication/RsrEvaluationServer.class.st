Class {
	#name : #RsrEvaluationServer,
	#superclass : #RsrAbstractEvaluationService,
	#category : #'RemoteServiceReplication-Public-Examples'
}

{ #category : #evaluating }
RsrEvaluationServer >> evaluate: aString [

	Transcript
		show: 'Server: ', aString;
		cr.
	^OpalCompiler new
		source: aString;
		evaluate
]
