Class {
	#name : #RsrReleaseObjects,
	#superclass : #RsrCommand,
	#instVars : [
		'oids'
	],
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #'instance creation' }
RsrReleaseObjects class >> oids: anArray [

	^self new
		oids: anArray;
		yourself
]

{ #category : #lifecycle }
RsrReleaseObjects >> encodeUsing: anRsrEncoder [

	encoding := anRsrEncoder encodeReleaseObjects: self
]

{ #category : #lifecycle }
RsrReleaseObjects >> executeFor: aConnection [

	| registry |
	registry := aConnection registry.
	oids do: [:oid | registry removeKey: oid]
]

{ #category : #accessing }
RsrReleaseObjects >> oids [

	^oids
]

{ #category : #accessing }
RsrReleaseObjects >> oids: anArray [

	oids := anArray
]

{ #category : #reporting }
RsrReleaseObjects >> reportOn: aLog [

	aLog debug: '', self class name, '/releasing/', self oids printString
]
