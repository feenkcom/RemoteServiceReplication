Class {
	#name : #RsrAbstractEvaluationService,
	#superclass : #RsrService,
	#category : #'RemoteServiceReplication-Examples-RemoteEvaluation'
}

{ #category : #accessing }
RsrAbstractEvaluationService class >> clientClass [

	^RsrEvaluationClient
]

{ #category : #accessing }
RsrAbstractEvaluationService class >> serverClass [

	^RsrEvaluationServer
]
