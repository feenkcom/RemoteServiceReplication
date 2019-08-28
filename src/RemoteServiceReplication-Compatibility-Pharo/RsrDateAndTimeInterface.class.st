Class {
	#name : #RsrDateAndTimeInterface,
	#superclass : #RsrObject,
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #accessing }
RsrDateAndTimeInterface class >> dateAndTimeClass [

	^DateAndTime
]

{ #category : #accessing }
RsrDateAndTimeInterface class >> fromMicroseconds: anInteger [
	"DateTime is encoded in offset +00:00"

	^self dateAndTimeClass
		fromSeconds: (anInteger / 1000000) + self secondsSincePosixEpoch
		offset: 0
]

{ #category : #accessing }
RsrDateAndTimeInterface class >> fromString: aString [

	^self dateAndTimeClass fromString: aString
]

{ #category : #accessing }
RsrDateAndTimeInterface class >> microsecondsSince: aDateTime [
	"DateTime is encoded in offset +00:00"

	| seconds |
	seconds := aDateTime asSeconds - self secondsSincePosixEpoch.
	^seconds * 1000000 + (aDateTime nanoSecond / 1000) rounded
]

{ #category : #accessing }
RsrDateAndTimeInterface class >> now [

	^self dateAndTimeClass now
]

{ #category : #accessing }
RsrDateAndTimeInterface class >> posixEpoch [

	^self dateAndTimeClass unixEpoch
]

{ #category : #accessing }
RsrDateAndTimeInterface class >> secondsSincePosixEpoch [

	^self posixEpoch asSeconds
]
