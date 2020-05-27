Class {
	#name : 'RsrServiceAbstractAllDataObjects',
	#superclass : 'RsrService',
	#instVars : [
		'positiveSmallInteger',
		'negativeSmallInteger',
		'integerZero',
		'string'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrServiceAbstractAllDataObjects class >> abstractClassName [

	^#RsrServiceAbstractAllDataObjects
]

{ #category : 'accessing' }
RsrServiceAbstractAllDataObjects class >> clientClassName [

	^#RsrClientAllDataObjects
]

{ #category : 'accessing' }
RsrServiceAbstractAllDataObjects class >> serverClassName [

	^#RsrServerAllDataObjects
]

{ #category : 'initialization' }
RsrServiceAbstractAllDataObjects >> initialize [

	super initialize.
	negativeSmallInteger := -1.
	integerZero := 0.
	positiveSmallInteger := 1.
	string := 'hello, world'
]
