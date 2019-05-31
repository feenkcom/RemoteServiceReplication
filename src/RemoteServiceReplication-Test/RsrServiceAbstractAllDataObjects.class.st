Class {
	#name : #RsrServiceAbstractAllDataObjects,
	#superclass : #RsrService,
	#instVars : [
		'positiveSmallInteger',
		'negativeSmallInteger',
		'integerZero',
		'string'
	],
	#category : #'RemoteServiceReplication-Test-Services'
}

{ #category : #accessing }
RsrServiceAbstractAllDataObjects class >> clientClass [

	^RsrClientAllDataObjects
]

{ #category : #accessing }
RsrServiceAbstractAllDataObjects class >> serverClass [

	^RsrServerAllDataObjects
]

{ #category : #initialization }
RsrServiceAbstractAllDataObjects >> initialize [

	super initialize.
	negativeSmallInteger := -1.
	integerZero := 0.
	positiveSmallInteger := 1.
	string := 'hello, world'
]
