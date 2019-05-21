decoding
decodeService: aStream
ofLength: anInteger

	| termination oid serviceName instance |
	termination := aStream position + anInteger.
	oid := self nextInteger: aStream.
	serviceName := self decodeObjectReference: aStream.
	instance := registry
		at: oid
		ifAbsent:
			[(self lookupClass: serviceName)
				rsrId: oid
				rsrRemoteInterface: self remoteInterface].
	self
		reflectedVariableNamesOf: instance
		do: [:name | instance instVarNamed: name put: (self decodeObjectReference: aStream)].
	termination = aStream position
		ifFalse: [self error: 'Invalid object encoding encountered'].
	registry addRemote: instance.
	^RsrRetainObject object: instance