public-accessing
at: aNameString
put: aService

	self
		register: aService
		for: aNameString.
	remoteSelf
		register: aService
		for: aNameString
		