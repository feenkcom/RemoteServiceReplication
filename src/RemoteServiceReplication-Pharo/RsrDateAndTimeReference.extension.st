Extension { #name : 'RsrDateAndTimeReference' }

{ #category : '*RemoteServiceReplication-Pharo' }
RsrDateAndTimeReference >> calculateFromMicroSeconds: anInteger [

	^DateAndTime
		fromSeconds: (anInteger / 1000000) + self posixEpoch asSeconds
		offset: 0
]

{ #category : '*RemoteServiceReplication-Pharo' }
RsrDateAndTimeReference >> microsecondsSinceEpoch: aDateAndTime [

	| seconds |
	seconds := aDateAndTime asSeconds - self posixEpoch asSeconds.
	^seconds * 1000000 + (aDateAndTime nanoSecond / 1000) rounded
]

{ #category : '*RemoteServiceReplication-Pharo' }
RsrDateAndTimeReference >> posixEpoch [

	^DateAndTime unixEpoch
]
