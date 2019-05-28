public-registration
at: aNameString
put: aService

	self
		register: aService
		for: aNameString.
	remoteSelf
		register: aService
		for: aNameString
		