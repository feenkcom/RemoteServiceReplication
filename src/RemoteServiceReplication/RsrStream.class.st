Class {
	#name : #RsrStream,
	#superclass : #Object,
	#instVars : [
		'stream'
	],
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #'instance creation' }
RsrStream class >> on: aStream [

	^self new
		stream: aStream;
		yourself
]

{ #category : #accessing }
RsrStream >> binary [

	stream binary
]

{ #category : #accessing }
RsrStream >> close [

	stream close
]

{ #category : #accessing }
RsrStream >> flush [

	stream flush
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
		ifTrue: [RsrConnectionClosed signal].
	^bytes
]

{ #category : #accessing }
RsrStream >> nextPutAll: aByteArray [

	^stream nextPutAll: aByteArray
]

{ #category : #accessing }
RsrStream >> stream: aStream [

	stream := aStream
]
