accessing
cleanup
	[ |orphans| 
		orphans := self findOrphanedKeys.
		self cleanupManagedObjects: orphans.
	] fork. "Don't want finalization to hang, so fork this and return.... though not sure that this is true"