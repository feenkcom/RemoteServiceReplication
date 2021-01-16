Extension { #name : 'RsrProcessModel' }

{ #category : '*RemoteServiceReplication-Pharo' }
RsrProcessModel class >> current [

	^current ifNil: [self resetCurrent]
]

{ #category : '*RemoteServiceReplication-Pharo' }
RsrProcessModel class >> current: concurrency [

	current := concurrency
]

{ #category : '*RemoteServiceReplication-Pharo' }
RsrProcessModel >> currentStackDump [

	| stream context |
	stream := WriteStream on: (String new: 128).
	context := thisContext.
	[context isNil]
		whileFalse:
			[stream
				print: context;
				lf.
			context := context sender].
	^stream contents
]

{ #category : '*RemoteServiceReplication-Pharo' }
RsrProcessModel class >> resetCurrent [

	^current := self new
]
