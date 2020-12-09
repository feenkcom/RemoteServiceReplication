Extension { #name : 'RsrDoubleReference' }

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

	^(aByteArray reversed changeClassTo: Float) * 1.0.
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDoubleReference >> convertToBytes: aFloat [

	^(aFloat _asFloat shallowCopy changeClassTo: ByteArray) reversed
]
