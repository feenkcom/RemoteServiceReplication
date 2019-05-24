decoding
decodeService: aStream

	| oid instVarCount serviceName instance |
	oid := self decodeControlWord: aStream.
	instVarCount := self decodeControlWord: aStream.
	serviceName := self decodeObjectReference: aStream.
	instance := registry
		at: oid
		ifAbsent:
			[(self lookupClass: serviceName)
				rsrId: oid
				rsrRemoteInterface: self remoteInterface].
	(self reflectedVariableNamesOf: instance) size = instVarCount
		ifFalse: [self error: 'Incorrectly encoded instance detected'].
	self
		reflectedVariableNamesOf: instance
		do: [:name | instance instVarNamed: name put: (self decodeObjectReference: aStream)].
	registry addRemote: instance.
	^instance