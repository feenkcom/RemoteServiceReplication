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
		encodeControlWord: self serviceType
		onto: aStream.
	self
		encodeControlWord: aService rsrId
		onto: aStream.
	self
		encodeControlWord: reflectedVariables size
		onto: aStream.
	self
		encodeSymbol: aService remoteServiceName
		onto: aStream.
	self
		reflectedVariablesOf: aService
		do: [:each | self encodeReferenceOf: each onto: aStream]