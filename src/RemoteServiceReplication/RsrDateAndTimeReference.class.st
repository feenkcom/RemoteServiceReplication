Class {
	#name : 'RsrDateAndTimeReference',
	#superclass : 'RsrValueReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'calculating' }
RsrDateAndTimeReference >> calculateFromMicroSeconds: anInteger [

    ^DateAndTime
        posixSeconds: (anInteger / 1000000)
        offset: Duration zero
]

{ #category : 'decoding' }
RsrDateAndTimeReference >> decode: aStream
using: aDecoder [

	| microseconds |
	microseconds := aDecoder decodeControlWord: aStream.
	value := self calculateFromMicroSeconds: microseconds
]
