Extension { #name : 'RsrConnection' }

{ #category : '*remoteservicereplication-gemstone' }
RsrConnection >> platformSpecificOpeningTasks [
	"Ensure that we are mourning ephemerons asynchronously."

	RsrAsyncMournHandler current ensureStarted
]
