accessing
decoder

	^decoder ifNil: [decoder := RsrDecoder registry: RsrRegistry new]