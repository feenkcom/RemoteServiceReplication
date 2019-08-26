Class {
	#name : #RsrDateTimeInterface,
	#superclass : #RsrObject,
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #accessing }
RsrDateTimeInterface class >> dateTimeClass [

	^DateAndTime
]

{ #category : #accessing }
RsrDateTimeInterface class >> fromMicroseconds: anInteger [
	"DateTime is encoded in offset +00:00"

	^self dateTimeClass
		fromSeconds: (anInteger / 1000000) + self secondsSincePosixEpoch
		offset: 0
]

{ #category : #accessing }
RsrDateTimeInterface class >> fromString: aString [

	^self dateTimeClass fromString: aString
]

{ #category : #accessing }
RsrDateTimeInterface class >> microsecondsSince: aDateTime [
	"DateTime is encoded in offset +00:00"

	| seconds |
	seconds := aDateTime asSeconds - self secondsSincePosixEpoch.
	^seconds * 1000000 + (aDateTime nanoSecond / 1000) rounded
]

{ #category : #accessing }
RsrDateTimeInterface class >> now [

	^self dateTimeClass now
]

{ #category : #accessing }
RsrDateTimeInterface class >> posixEpoch [

	^self dateTimeClass unixEpoch
]

{ #category : #accessing }
RsrDateTimeInterface class >> secondsSincePosixEpoch [

	^self posixEpoch asSeconds
]
