accessing
roots

	| roots |
	roots := IdentitySet new.
	roots
		add: self receiver;
		add: self selector;
		addAll: self arguments.
	^roots