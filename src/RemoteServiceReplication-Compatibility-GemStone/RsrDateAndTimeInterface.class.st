Class {
	#name : #RsrDateAndTimeInterface,
	#superclass : #RsrObject,
	#category : #'RemoteServiceReplication-Compatibility-GemStone'
}

{ #category : #accessing }
RsrDateAndTimeInterface class >> dateAndTimeClass [

	^DateAndTime
]

{ #category : #accessing }
RsrDateAndTimeInterface class >> fromMicroseconds: anInteger [
	"DateTime is encoded in offset +00:00"

    ^self dateAndTimeClass
        posixSeconds: (anInteger / 1000000)
        offset: Duration zero
]

{ #category : #accessing }
RsrDateAndTimeInterface class >> fromString: aString [

	^self dateAndTimeClass fromString: aString
]

{ #category : #accessing }
RsrDateAndTimeInterface class >> microsecondsSince: aDateAndTime [
	"DateTime is encoded in offset +00:00"

	^((aDateAndTime asSeconds - self posixEpoch asSeconds) * 1000000) rounded
]

{ #category : #accessing }
RsrDateAndTimeInterface class >> now [

	^self dateAndTimeClass now
]

{ #category : #accessing }
RsrDateAndTimeInterface class >> posixEpoch [

	^self dateAndTimeClass
        posixSeconds: 0
        offset: Duration zero
]
