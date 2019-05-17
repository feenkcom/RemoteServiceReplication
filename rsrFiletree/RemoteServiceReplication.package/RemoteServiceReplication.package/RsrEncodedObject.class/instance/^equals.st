comparing
= anEncodedObject

	self == anEncodedObject
		ifTrue: [^true].
	self class == anEncodedObject class
		ifFalse: [^false].
	^self object = anEncodedObject object
		and: [self encoding = anEncodedObject encoding]