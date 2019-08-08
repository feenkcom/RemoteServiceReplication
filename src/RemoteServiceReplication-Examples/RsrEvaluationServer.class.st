Class {
	#name : #RsrEvaluationServer,
	#superclass : #RsrAbstractEvaluationService,
	#category : #'RemoteServiceReplication-Examples-RemoteEvaluation'
}

{ #category : #evaluating }
RsrEvaluationServer >> evaluate: aString [

	^OpalCompiler new
		source: aString;
		evaluate
]
