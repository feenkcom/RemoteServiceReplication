Class {
	#name : #RsrAbstractEvaluationService,
	#superclass : #RsrService,
	#category : #'RemoteServiceReplication-Public'
}

{ #category : #accessing }
RsrAbstractEvaluationService class >> clientClass [

	^RsrEvaluationClient
]

{ #category : #accessing }
RsrAbstractEvaluationService class >> serverClass [

	^RsrEvaluationServer
]
