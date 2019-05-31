Class {
	#name : #RsrStream,
	#superclass : #ProtoObject,
	#instVars : [
		'stream'
	],
	#category : 'RemoteServiceReplication-Communications'
}

{ #category : #'instance creation' }
RsrStream class >> on: aStream [

	^self new
		stream: aStream;
		yourself
]

{ #category : #accessing }
RsrStream >> doesNotUnderstand: aMessage [

	^aMessage sendTo: stream
]

{ #category : #accessing }
RsrStream >> next [

	^self next: 1
]

{ #category : #accessing }
RsrStream >> next: aLength [

	| bytes |
	bytes := stream next: aLength.
	bytes size ~~ aLength
		ifTrue: [ConnectionClosed signal].
	^bytes
]

{ #category : #accessing }
RsrStream >> stream: aStream [

	stream := aStream
]
