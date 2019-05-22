comparing
= anRsrSendMessage

	self == anRsrSendMessage
		ifTrue: [^true].
	self class == anRsrSendMessage class
		ifFalse: [^false].
	^self transaction = anRsrSendMessage transaction
		and: [self receiver = anRsrSendMessage receiver
			and: [self selector = anRsrSendMessage selector
				and: [self arguments = anRsrSendMessage arguments]]]