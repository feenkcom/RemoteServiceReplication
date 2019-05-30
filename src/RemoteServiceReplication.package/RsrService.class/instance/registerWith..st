private-lifecycle
registerWith: aRegistry

	self isServer
		ifTrue: [aRegistry retain: self]
		ifFalse: [aRegistry register: self]