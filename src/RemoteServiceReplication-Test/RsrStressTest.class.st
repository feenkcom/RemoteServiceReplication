Class {
	#name : 'RsrStressTest',
	#superclass : 'RsrSystemTestCase',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrStressTest >> repetitions [

	^1000
]

{ #category : 'running' }
RsrStressTest >> testRepeatedSendReceive1KBytes [

	| client bytes |
	client := connectionA serviceFor: #RsrChattyClient.
	bytes := ByteArray new: 1024.
	self repetitions timesRepeat: [client returnArgument:  bytes].
	self assert: true. "If we get to this point, the sends have all successed"
]

{ #category : 'running' }
RsrStressTest >> testRepeatedSendReceive1MBytes [

	| client bytes |
	client := connectionA serviceFor: #RsrChattyClient.
	bytes := ByteArray new: 1024 squared.
	RsrLog new debug: 'About to start writing bytes'.
	self repetitions timesRepeat: [client returnArgument:  bytes].
	self assert: true. "If we get to this point, the sends have all successed"
]

{ #category : 'running' }
RsrStressTest >> testRepeatedSendReceive2KBytes [

	| client bytes |
	client := connectionA serviceFor: #RsrChattyClient.
	bytes := ByteArray new: 1024 * 2.
	self repetitions timesRepeat: [client returnArgument:  bytes].
	self assert: true. "If we get to this point, the sends have all successed"
]

{ #category : 'running' }
RsrStressTest >> testRepeatedUnarySends [

	| client |
	client := connectionA serviceFor: #RsrChattyClient.
	self repetitions timesRepeat: [client returnSelf].
	self assert: true. "If we get to this point, the sends have all successed"
]
