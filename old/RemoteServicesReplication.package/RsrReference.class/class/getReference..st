instance creation
getReference: anObject
	| rsrReference |
	rsrReference := RsrReference new.
	rsrReference put: anObject.
	^rsrReference