Class {
	#name : #RsrDateAndTimeSpecies,
	#superclass : #RsrSpecies,
	#category : #'RemoteServiceReplication-Base-Species'
}

{ #category : #coding }
RsrDateAndTimeSpecies class >> encode: aDateAndTime
with: anEncoder
onto: aStream [

	^anEncoder
		encodeDateAndTime: aDateAndTime
		on: aStream
]
