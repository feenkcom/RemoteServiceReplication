Class {
	#name : #RsrEvaluationClientTest,
	#superclass : #RsrSystemTestCase,
	#instVars : [
		'service'
	],
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #accessing }
RsrEvaluationClientTest >> literals [

	^#( nil true false -1 0 1 )
]

{ #category : #running }
RsrEvaluationClientTest >> setUp [

	super setUp.
	service := connectionA serviceFor: #RsrEvaluationClient
]

{ #category : #running }
RsrEvaluationClientTest >> testEvaluteLiterals [

	self literals
		do:
			[:each | | result |
			result := service evaluate: each printString.
			self
				assert: result
				equals: each]
]

{ #category : #running }
RsrEvaluationClientTest >> testEvaluteService [

	| result |
	result := service evaluate: 'RsrEvaluationServer new'.
	self
		assert: result class
		equals: RsrEvaluationClient
]
