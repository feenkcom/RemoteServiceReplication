Class {
	#name : 'RsrServiceAllDataObjects',
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
RsrServiceAllDataObjects class >> clientClassName [

	^#RsrClientAllDataObjects
]

{ #category : 'accessing' }
RsrServiceAllDataObjects class >> serverClassName [

	^#RsrServerAllDataObjects
]

{ #category : 'accessing' }
RsrServiceAllDataObjects class >> templateClassName [

	^#RsrServiceAllDataObjects
]

{ #category : 'initialization' }
RsrServiceAllDataObjects >> initialize [

	super initialize.
	negativeSmallInteger := -1.
	integerZero := 0.
	positiveSmallInteger := 1.
	string := 'hello, world'
]
