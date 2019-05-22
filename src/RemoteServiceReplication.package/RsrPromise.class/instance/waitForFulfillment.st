private-waiting
waitForFulfillment

	mutex isSignaled
		ifTrue: [^self].
	mutex wait.
	mutex signal