Class {
	#name : #RsrNullSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrNullSpecies class >> decode: bytes [

	self shouldNotImplement
]

{ #category : #coding }
RsrNullSpecies class >> encode: anObject
with: anEncoder
onto: aStream [

	^RsrUnsupportedObject signal: anObject
]
