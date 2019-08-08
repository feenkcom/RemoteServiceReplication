Class {
	#name : #RsrEvaluationClient,
	#superclass : #RsrAbstractEvaluationService,
	#category : #'RemoteServiceReplication-Examples-RemoteEvaluation'
}

{ #category : #evaluating }
RsrEvaluationClient >> evaluate: aString [

	^remoteSelf evaluate: aString
]
