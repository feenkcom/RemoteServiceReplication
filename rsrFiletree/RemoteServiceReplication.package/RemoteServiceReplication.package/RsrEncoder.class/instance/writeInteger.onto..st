private-writing
writeInteger: anInteger
onto: aStream

	| encodedBytes |
	encodedBytes := anInteger asByteArrayOfSize: self sizeOfInteger.
	aStream nextPutAll: encodedBytes