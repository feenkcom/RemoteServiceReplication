Extension { #name : 'RsrDateAndTimeSpecies' }

{ #category : '*RemoteServiceReplication-Pharo' }
RsrDateAndTimeSpecies class >> fromMicroseconds: anInteger [

	^DateAndTime
		fromSeconds: (anInteger / 1000000) + self posixEpoch asSeconds
		offset: 0
]

{ #category : '*RemoteServiceReplication-Pharo' }
RsrDateAndTimeSpecies class >> microsecondsSinceEpoch: aDateAndTime [

	| seconds |
	seconds := aDateAndTime asSeconds - self posixEpoch asSeconds.
	^seconds * 1000000 + (aDateAndTime nanoSecond / 1000) rounded
]

{ #category : '*RemoteServiceReplication-Pharo' }
RsrDateAndTimeSpecies class >> now [
	"Return an instance of the native DateAndTime class representing the current instant"

	^DateAndTime now
]

{ #category : '*RemoteServiceReplication-Pharo' }
RsrDateAndTimeSpecies class >> posixEpoch [

	^DateAndTime unixEpoch
]
