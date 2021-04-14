Extension { #name : 'RsrToken' }

{ #category : '*remoteservicereplication-gemstone' }
RsrToken class >> newRandom [
	"Create a new Token with random bytes."

	| random bytes |
	random := HostRandom new.
	bytes := ByteArray new: 16.
	bytes unsigned32At: 1 put: random integer.
	bytes unsigned32At: 5 put: random integer.
	bytes unsigned32At: 9 put: random integer.
	bytes unsigned32At: 13 put: random integer.
	^self bytes: bytes
]
