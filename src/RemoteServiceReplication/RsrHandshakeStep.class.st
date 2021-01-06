Class {
	#name : 'RsrHandshakeStep',
	#superclass : 'RsrObject',
	#instVars : [
		'stream'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrHandshakeStep class >> over: aStream [

	^self new
		stream: aStream;
		yourself
]

{ #category : 'performing' }
RsrHandshakeStep >> perform [
	"Perform the work for this step."

	^self subclassResponsibility
]

{ #category : 'accessing' }
RsrHandshakeStep >> stream [
	"The stream that should be used by the step for its action."

	^stream
]

{ #category : 'accessing' }
RsrHandshakeStep >> stream: aStream [
	"The stream that should be used by the step for its action."

	stream := aStream
]
