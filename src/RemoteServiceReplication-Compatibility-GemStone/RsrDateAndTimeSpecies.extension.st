Extension { #name : 'RsrDateAndTimeSpecies' }

{ #category : '*remoteservicereplication-compatibility-gemstone' }
RsrDateAndTimeSpecies class >> fromMicroseconds: anInteger [

    ^DateAndTime
        posixSeconds: (anInteger / 1000000)
        offset: Duration zero
]

{ #category : '*remoteservicereplication-compatibility-gemstone' }
RsrDateAndTimeSpecies class >> microsecondsSinceEpoch: aDateAndTime [

	^((aDateAndTime asSeconds - self posixEpoch asSeconds) * 1000000) rounded
]

{ #category : '*remoteservicereplication-compatibility-gemstone' }
RsrDateAndTimeSpecies class >> now [

	^DateAndTime now
]

{ #category : '*remoteservicereplication-compatibility-gemstone' }
RsrDateAndTimeSpecies class >> posixEpoch [

	^DateAndTime
        posixSeconds: 0
        offset: Duration zero
]
