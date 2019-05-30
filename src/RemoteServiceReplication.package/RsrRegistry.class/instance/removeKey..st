adding/removing
removeKey: anRsrId

	| value |
	mutex
		critical:
			[value := map
				removeKey: anRsrId
				ifAbsent: [^nil].
			storage remove: value].
	^value