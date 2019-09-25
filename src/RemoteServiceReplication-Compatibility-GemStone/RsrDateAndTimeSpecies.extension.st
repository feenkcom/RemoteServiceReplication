Extension { #name : #RsrDateAndTimeSpecies }

{ #category : #accessing }
RsrDateAndTimeSpecies class >> fromMicroseconds: anInteger [

    ^DateAndTime
        posixSeconds: (anInteger / 1000000)
        offset: Duration zero
]

{ #category : #accessing }
RsrDateAndTimeSpecies class >> microsecondsSinceEpoch: aDateAndTime [

	^((aDateAndTime asSeconds - self posixEpoch asSeconds) * 1000000) rounded
]

{ #category : #accessing }
RsrDateAndTimeSpecies class >> now [

	^DateAndTime now
]

{ #category : #accessing }
RsrDateAndTimeSpecies class >> posixEpoch [

	^DateAndTime
        posixSeconds: 0
        offset: Duration zero
]
