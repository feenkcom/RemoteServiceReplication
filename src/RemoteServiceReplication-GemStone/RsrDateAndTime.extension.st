Extension { #name : 'RsrDateAndTime' }

{ #category : '*remoteservicereplication-gemstone' }
RsrDateAndTime class >> fromMicroseconds: anInteger [

    ^DateAndTime
        posixSeconds: (anInteger / 1000000)
        offset: Duration zero
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDateAndTime class >> microsecondsSinceEpoch: aDateAndTime [

	^((aDateAndTime asSeconds - self posixEpoch asSeconds) * 1000000) rounded
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDateAndTime class >> now [

	^DateAndTime now
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDateAndTime class >> posixEpoch [

	^DateAndTime
        posixSeconds: 0
        offset: Duration zero
]
