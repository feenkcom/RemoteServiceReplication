Class {
	#name : #RsrEvaluationClient,
	#superclass : #RsrAbstractEvaluationService,
	#category : #'RemoteServiceReplication-Public-Examples'
}

{ #category : #evaluating }
RsrEvaluationClient >> evaluate: aString [

	Transcript
		show: 'Client: ', aString;
		cr.
	^remoteSelf evaluate: aString
]
