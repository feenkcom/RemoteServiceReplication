Extension { #name : 'RsrDoubleReference' }

{ #category : '*remoteservicereplication-gemstone' }
RsrDoubleReference class >> convertToBytes: aFloat [

	| bytes |
	bytes := ByteArray new: 8.
	bytes
		doubleAt: 1
		put: aFloat.
	^bytes
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDoubleReference class >> infinity [

	^Float fromString: 'Infinity'
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDoubleReference class >> nan [

	^Float fromString: '-NaN'
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDoubleReference >> convertBytes: aByteArray [

	^aByteArray doubleAt: 1
]
