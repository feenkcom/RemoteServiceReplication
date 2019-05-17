private-finalization
finalize

	self isMirrored
		ifTrue:
			[self connection release: self.
			self connection: nil].