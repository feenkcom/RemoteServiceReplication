Class {
	#name : 'RsrReleaseServices',
	#superclass : 'RsrCommand',
	#instVars : [
		'oids'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrReleaseServices class >> oids: anArray [

	^self new
		oids: anArray;
		yourself
]

{ #category : 'lifecycle' }
RsrReleaseServices >> encodeUsing: anRsrEncoder [

	encoding := anRsrEncoder encodeReleaseObjects: self
]

{ #category : 'lifecycle' }
RsrReleaseServices >> executeFor: aConnection [

	| registry |
	registry := aConnection registry.
	oids do: [:oid | registry cleanupEntryFor: oid]
]

{ #category : 'accessing' }
RsrReleaseServices >> oids [

	^oids
]

{ #category : 'accessing' }
RsrReleaseServices >> oids: anArray [

	oids := anArray
]

{ #category : 'reporting' }
RsrReleaseServices >> reportOn: aLog [

	aLog debug: 'RsrReleaseObjects(', self oids printString, ')'
]
