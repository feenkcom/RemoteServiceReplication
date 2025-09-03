Extension { #name : #RsrReference }

{ #category : #'*remoteservicereplication' }
RsrReference class >> referenceClassFor: anObject [

	(anObject isKindOf: RsrService)
		ifTrue: [^RsrServiceReference].
	^self referenceMapping
		at: anObject class
		ifAbsent: [RsrUnsupportedObject signal: anObject]
]
