Extension { #name : 'RsrSpecies' }

{ #category : '*remoteservicereplication' }
RsrSpecies class >> speciesOf: anObject [

	(anObject isKindOf: RsrService)
		ifTrue: [^RsrServiceSpecies].
	anObject == true
		ifTrue: [^RsrTrueSpecies].
	anObject == false
		ifTrue: [^RsrFalseSpecies].
	(anObject isKindOf: Integer)
		ifTrue: [^anObject positive ifTrue: [RsrPositiveIntegerSpecies] ifFalse: [RsrNegativeIntegerSpecies]].
	^self speciesMapping
		at: anObject class
		ifAbsent: [RsrSpecies nullSpecies]
]
