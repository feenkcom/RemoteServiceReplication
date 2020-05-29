Class {
	#name : 'RsrReleaseObjects',
	#superclass : 'RsrCommand',
	#instVars : [
		'oids'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrReleaseObjects class >> oids: anArray [

	^self new
		oids: anArray;
		yourself
]

{ #category : 'lifecycle' }
RsrReleaseObjects >> encodeUsing: anRsrEncoder [

	encoding := anRsrEncoder encodeReleaseObjects: self
]

{ #category : 'lifecycle' }
RsrReleaseObjects >> executeFor: aConnection [

	| registry |
	registry := aConnection registry.
	oids do: [:oid | registry cleanupEntryFor: oid]
]

{ #category : 'accessing' }
RsrReleaseObjects >> oids [

	^oids
]

{ #category : 'accessing' }
RsrReleaseObjects >> oids: anArray [

	oids := anArray
]

{ #category : 'reporting' }
RsrReleaseObjects >> reportOn: aLog [

	aLog debug: 'RsrReleaseObjects(', self oids printString, ')'
]
