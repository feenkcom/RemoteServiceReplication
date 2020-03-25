!SequenceableCollection methodsFor!
doWithIndex: aBlock

	| index size |
	index := 1.
	size := self size.
	[index <= size]
		whileTrue:
			[aBlock
				value: (self at: index)
				value: index.
			index := index + 1]! !