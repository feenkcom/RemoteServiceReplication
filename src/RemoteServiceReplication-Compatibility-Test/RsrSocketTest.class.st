Class {
	#name : #RsrSocketTest,
	#superclass : #RsrTestCase,
	#category : #'RemoteServiceReplication-Compatibility-Test'
}

{ #category : #running }
RsrSocketTest >> testConnectToInvalidPort [

	self assert: false
]
