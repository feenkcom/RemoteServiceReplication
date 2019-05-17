private-writing
writeInteger: anInteger
onto: aStream

	| encodedBytes |
	encodedBytes := anInteger asByteArrayOfSize: 8.
	aStream nextPutAll: encodedBytes