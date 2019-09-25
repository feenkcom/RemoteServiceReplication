Class {
	#name : #RsrDateAndTimeSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrDateAndTimeSpecies class >> encodeReference: aDateAndTime
with: anEncoder
onto: aStream [

	^anEncoder
		encodeDateAndTime: aDateAndTime
		onto: aStream
]

{ #category : #accessing }
RsrDateAndTimeSpecies class >> speciesIdentifier [

	^14
]
