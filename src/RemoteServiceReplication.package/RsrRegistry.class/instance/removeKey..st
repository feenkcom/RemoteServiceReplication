adding/removing
removeKey: anRsrId

	| value |
	value := map
		removeKey: anRsrId
		ifAbsent: [^nil].
	storage remove: value.
	^value