Class {
	#name : #RsrEvaluationServer,
	#superclass : #RsrAbstractEvaluationService,
	#category : #'RemoteServiceReplication-Public-Examples'
}

{ #category : #evaluating }
RsrEvaluationServer >> evaluate: aString [

	^OpalCompiler new
		source: aString;
		evaluate
]
