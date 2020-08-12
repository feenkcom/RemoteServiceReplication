Class {
	#name : 'RsrStressTest',
	#superclass : 'RsrSystemTestCase',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrStressTest class >> defaultTimeLimit [

	^20 seconds
]

{ #category : 'running-utilities' }
RsrStressTest >> repeatedlySend: anObject [

	| client server |
	client := connectionA serviceFor: #RsrRemoteAction.
	server := connectionB registry serviceAt: client _id.
	server action: [:object | object].
	self repetitions timesRepeat: [client value: anObject].
	self assert: true. "If we get to this point, the sends have all successed"
]

{ #category : 'accessing' }
RsrStressTest >> repetitions [

	^1000
]

{ #category : 'running' }
RsrStressTest >> testRepeatedSendReceive1KBytes [

	self repeatedlySend: (ByteArray new: 1024)
]

{ #category : 'running' }
RsrStressTest >> testRepeatedSendReceive1MBytes [

	self repeatedlySend: (ByteArray new: 1024 squared)
]

{ #category : 'running' }
RsrStressTest >> testRepeatedSendReceive2KBytes [

	self repeatedlySend: (ByteArray new: 1024 *2)
]

{ #category : 'running' }
RsrStressTest >> testRepeatedUnarySends [

	| client server |
	client := connectionA serviceFor: #RsrRemoteAction.
	server := connectionB registry serviceAt: client _id.
	server action: [server].
	self repetitions timesRepeat: [client value].
	self assert: true. "If we get to this point, the sends have all successed"
]
