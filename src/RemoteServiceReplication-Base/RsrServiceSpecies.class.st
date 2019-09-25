Class {
	#name : #RsrServiceSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrServiceSpecies class >> decodeWith: aDecoder
from: aStream [
	"Decode the provided bytes into the default native class for this species"

	| oid instVarCount serviceName instance |
	oid := aDecoder decodeControlWord: aStream.
	instVarCount := aDecoder decodeControlWord: aStream.
	serviceName := aDecoder decodeObjectReference: aStream.
	instance := aDecoder registry
		at: oid
		ifAbsent:
			[((aDecoder lookupClass: serviceName)
				_id: oid
				_connection: aDecoder connection)
					yourself].
	instance registerWith: aDecoder registry.
	(RsrReflection reflectedVariableNamesOf: instance) size = instVarCount
		ifFalse: [self error: 'Incorrectly encoded instance detected'].
	RsrReflection
		reflectedVariableIndecesOf: instance
		do: [:index | instance instVarAt: index put: (aDecoder decodeObjectReference: aStream)].
	^instance
]

{ #category : #coding }
RsrServiceSpecies class >> encodeReference: aService
with: anEncoder
onto: aStream [

	anEncoder
		encodeControlWord: aService _id
		onto: aStream
]

{ #category : #accessing }
RsrServiceSpecies class >> speciesIdentifier [

	^0
]
