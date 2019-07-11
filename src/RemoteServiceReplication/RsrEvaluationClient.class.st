Class {
	#name : #RsrEvaluationClient,
	#superclass : #RsrAbstractEvaluationService,
	#category : #'RemoteServiceReplication-Public-Examples'
}

{ #category : #evaluating }
RsrEvaluationClient >> evaluate: aString [

	^remoteSelf evaluate: aString
]
