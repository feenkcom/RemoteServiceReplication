private-encoding
encodeService: aService
on: aStream

	"type"
	"the OID for the object"
	"the name of the remote service to create"
	"Write the object slots"
	self writeServiceTypeOnto: aStream.
	self
		writeInteger: aService rsrId
		onto: aStream.
	self
		writeSymbol: aService remoteServiceName
		onto: aStream