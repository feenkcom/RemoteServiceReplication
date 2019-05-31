Class {
	#name : #RsrCommandDispatcher,
	#superclass : #RsrCommandProcessor,
	#instVars : [
		'queue'
	],
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #dispatching }
RsrCommandDispatcher >> dispatch: aCommand [

	queue nextPut: aCommand
]

{ #category : #running }
RsrCommandDispatcher >> executeCycle [

	queue next executeFor: connection
]

{ #category : #initialize }
RsrCommandDispatcher >> initialize [

	super initialize.
	queue := SharedQueue new
]

{ #category : #accessing }
RsrCommandDispatcher >> priority [

	^Processor lowIOPriority - 1
]
