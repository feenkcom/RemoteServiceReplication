Class {
	#name : #RsrDateTimeInterface,
	#superclass : #RsrObject,
	#category : #'RemoteServiceReplication-Compatibility'
}

{ #category : #accessing }
RsrDateTimeInterface class >> fromMicroseconds: anInteger [

	^DateAndTime
		fromSeconds: (anInteger / 1000000) + self secondsSincePosixEpoch
		offset: 0
]

{ #category : #accessing }
RsrDateTimeInterface class >> microsecondsSince: aDateTime [

	| seconds |
	seconds := aDateTime asSeconds - self secondsSincePosixEpoch.
	^seconds * 1000000 + (aDateTime nanoSecond / 1000) rounded
]

{ #category : #accessing }
RsrDateTimeInterface class >> posixEpoch [

	^DateAndTime unixEpoch
]

{ #category : #accessing }
RsrDateTimeInterface class >> secondsSincePosixEpoch [

	^self posixEpoch asSeconds
]
