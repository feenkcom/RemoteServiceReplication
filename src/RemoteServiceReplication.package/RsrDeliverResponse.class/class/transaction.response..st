instance creation
transaction: aTransactionId
response: anObject

	^self new
		transaction: aTransactionId;
		response: anObject;
		yourself