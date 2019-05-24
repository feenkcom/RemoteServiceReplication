private-encoding
encodeService: aService
on: aStream

	"type"
	"the OID for the object"
	"the name of the remote service to create"
	"Write the object slots"
	| reflectedVariables |
	reflectedVariables := self reflectedVariableNamesOf: aService.
	self
		writeControlWord: self serviceType
		onto: aStream.
	self
		writeControlWord: aService rsrId
		onto: aStream.
	self
		writeControlWord: reflectedVariables size
		onto: aStream.
	self
		writeSymbol: aService remoteServiceName
		onto: aStream.
	self
		reflectedVariablesOf: aService
		do: [:each | self encodeReferenceOf: each onto: aStream]