Extension { #name : #RsrProcessModel }

{ #category : #'*RemoteServiceReplication-Pharo' }
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
