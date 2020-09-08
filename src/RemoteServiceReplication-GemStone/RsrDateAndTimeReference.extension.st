Extension { #name : 'RsrDateAndTimeReference' }

{ #category : '*remoteservicereplication-gemstone' }
RsrDateAndTimeReference >> calculateFromMicroSeconds: anInteger [

    ^DateAndTime
        posixSeconds: (anInteger / 1000000)
        offset: Duration zero
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDateAndTimeReference >> microsecondsSinceEpoch: aDateAndTime [

	^((aDateAndTime asSeconds - self posixEpoch asSeconds) * 1000000) rounded
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDateAndTimeReference >> posixEpoch [

	^DateAndTime
        posixSeconds: 0
        offset: Duration zero
]
