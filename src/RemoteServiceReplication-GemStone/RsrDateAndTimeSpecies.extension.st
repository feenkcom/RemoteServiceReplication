Extension { #name : 'RsrDateAndTimeSpecies' }

{ #category : '*remoteservicereplication-gemstone' }
RsrDateAndTimeSpecies class >> fromMicroseconds: anInteger [

    ^DateAndTime
        posixSeconds: (anInteger / 1000000)
        offset: Duration zero
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDateAndTimeSpecies class >> microsecondsSinceEpoch: aDateAndTime [

	^((aDateAndTime asSeconds - self posixEpoch asSeconds) * 1000000) rounded
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDateAndTimeSpecies class >> now [

	^DateAndTime now
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDateAndTimeSpecies class >> posixEpoch [

	^DateAndTime
        posixSeconds: 0
        offset: Duration zero
]
