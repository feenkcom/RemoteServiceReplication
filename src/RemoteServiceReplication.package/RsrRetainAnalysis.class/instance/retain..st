actions
retain: anRsrObject

	| retainCommand |
	retainCommand := RsrRetainObject object: anRsrObject.
	retainCommand encodeUsing: encoder.
	self retainCommands add: retainCommand