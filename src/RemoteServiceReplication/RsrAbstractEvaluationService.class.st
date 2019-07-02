Class {
	#name : #RsrAbstractEvaluationService,
	#superclass : #RsrService,
	#category : #'RemoteServiceReplication-Public-Examples'
}

{ #category : #accessing }
RsrAbstractEvaluationService class >> clientClass [

	^RsrEvaluationClient
]

{ #category : #accessing }
RsrAbstractEvaluationService class >> serverClass [

	^RsrEvaluationServer
]
