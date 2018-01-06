receiving
deserializeAndProcess: serializedData
	|convertedData|
	convertedData := self deSerializeStream: serializedData.
	^self processDeserializedObject: convertedData.