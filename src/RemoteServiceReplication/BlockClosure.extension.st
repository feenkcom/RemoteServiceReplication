Extension { #name : #BlockClosure }

{ #category : #'*RemoteServiceReplication' }
BlockClosure >> async [

	| promise |
	promise := RsrPromise new.
	[promise fulfill: self value] fork.
	Processor yield.
	^promise
]
