accessing
next: aLength

	| bytes |
	bytes := stream next: aLength.
	bytes size ~~ aLength
		ifTrue: [ConnectionClosed signal].
	^bytes