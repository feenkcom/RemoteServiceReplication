accessing
next: aCount

	| result |
	result := Array new: aCount.
	1 to: aCount do: [:i | result at: i put: self next].
	^result