accessing
decoder

	^decoder ifNil: [decoder := RsrDecoder registry: (registry := RsrRegistry new) connection: RsrMockConnection new]