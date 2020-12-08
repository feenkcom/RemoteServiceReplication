Extension { #name : 'RsrDoubleReference' }

{ #category : '*remoteservicereplication-gemstone' }
RsrDoubleReference class >> negativeInfinity [

	^Float fromString: '-Infinity'
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDoubleReference class >> negativeNan [

	^Float fromString: '-NaN'
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDoubleReference class >> positiveInfinity [

	^Float fromString: 'Infinity'
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDoubleReference class >> positiveNan [

	^Float fromString: 'NaN'
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDoubleReference >> convertBytes: aByteArray [

	^(aByteArray reversed changeClassTo: Float) * 1.0.
]

{ #category : '*remoteservicereplication-gemstone' }
RsrDoubleReference >> convertToBytes: aFloat [

	^(aFloat _asFloat shallowCopy changeClassTo: ByteArray) reversed
]
