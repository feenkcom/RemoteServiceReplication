public-accessing
at: aNameString
ifAbsent: aBlock

	^self
		critical:
			[services
				at: aNameString
				ifAbsent: aBlock]