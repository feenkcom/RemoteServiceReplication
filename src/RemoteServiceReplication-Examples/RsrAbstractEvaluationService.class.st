Class {
	#name : #RsrAbstractEvaluationService,
	#superclass : #RsrService,
	#category : #'RemoteServiceReplication-Examples-RemoteEvaluation'
}

{ #category : #accessing }
RsrAbstractEvaluationService class >> clientClassName [

	^#RsrEvaluationClient
]

{ #category : #accessing }
RsrAbstractEvaluationService class >> serverClassName [

	^#RsrEvaluationServer
]
