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
        posixSeconds: (anInteger / 1000000)
        offset: Duration zero
]

{ #category : #accessing }
RsrDateTimeInterface class >> fromString: aString [

	^self dateTimeClass fromString: aString
]

{ #category : #accessing }
RsrDateTimeInterface class >> microsecondsSince: aDateAndTime [
	"DateTime is encoded in offset +00:00"

	^((aDateAndTime asSeconds - self posixEpoch asSeconds) * 1000000) rounded
]

{ #category : #accessing }
RsrDateTimeInterface class >> now [

	^self dateTimeClass now
]

{ #category : #accessing }
RsrDateTimeInterface class >> posixEpoch [

	^self dateTimeClass
        posixSeconds: 0
        offset: Duration zero
]
