tests
testSerializationHappyPath
	|rb serializedData deserializedData stringToTest|
	stringToTest := 'aNotSoRandomString'.
	rb := RsrBroker new.
	serializedData := rb serialize: stringToTest.
	deserializedData := rb deSerializeStream: serializedData.
	self assert: stringToTest equals: deserializedData.