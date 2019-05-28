accessing
value: anObject

	value := anObject.
	self
		markDirty;
		synchronize