instance creation
on: anRsrObject

	| instance |
	instance := self new.
	instance rsrObject: anRsrObject.
	^instance