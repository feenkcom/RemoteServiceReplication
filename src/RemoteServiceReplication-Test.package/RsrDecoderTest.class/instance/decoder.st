accessing
decoder

	^decoder ifNil: [decoder := RsrDecoder registry: (registry := RsrRegistry oidSpigot: RsrNumericSpigot naturals) connection: RsrMockConnection new]