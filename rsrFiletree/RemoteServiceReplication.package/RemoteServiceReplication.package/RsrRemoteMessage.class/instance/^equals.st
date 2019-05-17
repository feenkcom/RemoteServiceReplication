comparing
= aRemoteMessage

	self == aRemoteMessage
		ifTrue: [^true].
	self class == aRemoteMessage class
		ifFalse: [^false].
	^self receiver = aRemoteMessage receiver
		and: [self selector = aRemoteMessage selector
				and: [self arguments = aRemoteMessage arguments
						and: [self transactionId = aRemoteMessage transactionId]]]