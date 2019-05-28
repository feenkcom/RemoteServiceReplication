private-finalization
finalize

	self isMirrored
		ifFalse: [^self].
	"rsrConnection release: rsrId."
	rsrConnection := nil