accessing
findOrphanedKeys
	|entriesToClean|
	entriesToClean := LinkedList new.
	managedObjects  
		ifNotNil: [ 
			managedObjects ifNotEmpty: [ 
				managedObjects  keysDo: [ :key |
					(managedObjects at: key )	
						ifNil: [ entriesToClean add: key.]
					 ].
			]
		].
	^entriesToClean