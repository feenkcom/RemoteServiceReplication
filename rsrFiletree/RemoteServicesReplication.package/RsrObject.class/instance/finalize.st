instance creation
finalize
	|rsr|
	rsr := RsrRegistry uniqueInstance.
	rsr cleanup.
	Transcript show: self rsrName ;show: ' finalized.' ; cr.

