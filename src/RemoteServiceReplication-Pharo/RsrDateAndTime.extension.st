Extension { #name : #RsrDateAndTime }

{ #category : #'*RemoteServiceReplication-Pharo' }
RsrDateAndTime class >> fromMicroseconds: anInteger [

	^DateAndTime
		fromSeconds: (anInteger / 1000000) + self posixEpoch asSeconds
		offset: 0
]

{ #category : #'*RemoteServiceReplication-Pharo' }
RsrDateAndTime class >> microsecondsSinceEpoch: aDateAndTime [

	| seconds |
	seconds := aDateAndTime asSeconds - self posixEpoch asSeconds.
	^seconds * 1000000 + (aDateAndTime nanoSecond / 1000) rounded
]

{ #category : #'*RemoteServiceReplication-Pharo' }
RsrDateAndTime class >> now [

	^DateAndTime now
]

{ #category : #'*RemoteServiceReplication-Pharo' }
RsrDateAndTime class >> posixEpoch [

	^DateAndTime unixEpoch
]
