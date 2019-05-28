private-registration
register: aService
for: aNameString

	self
		critical:
			[services
				at: aNameString
				put: aService]