Class {
	#name : #RsrServiceSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrServiceSpecies class >> decodeReference: aStream
for: aDecoder [
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
				connection: aDecoder connection)
					yourself].
	instance _addTo: aDecoder registry.
	instance variablesToReflect size = instVarCount
		ifFalse: [self error: 'Incorrectly encoded instance detected'].
	instance _reflectedVariableIndecesDo: [:index | instance instVarAt: index put: (aDecoder decodeObjectReference: aStream)].
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
