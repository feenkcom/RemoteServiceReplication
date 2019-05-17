receiving
deserializeAndExecuteObjectServerMethod: serializedData
	|convertedData|
	convertedData := self deSerializeStream: serializedData.
	^self executeObjectServerMethod: convertedData.