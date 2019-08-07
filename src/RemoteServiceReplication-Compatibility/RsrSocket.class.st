Class {
	#name : #RsrSocket,
	#superclass : #Object,
	#instVars : [
		'socket'
	],
	#category : #'RemoteServiceReplication-Compatibility'
}

{ #category : #reading }
RsrSocket >> read: aCount [

	^socket read: aCount
]
