actions
retain: anRsrObject

	| retainCommand |
	retainCommand := RsrRetainObject object: anRsrObject.
	retainCommand encodeUsing: self encoder.
	self retainCommands add: retainCommand