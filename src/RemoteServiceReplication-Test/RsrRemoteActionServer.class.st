Class {
	#name : 'RsrRemoteActionServer',
	#superclass : 'RsrRemoteAction',
	#instVars : [
		'action',
		'debugHandler'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrRemoteActionServer >> action [

	^action
]

{ #category : 'accessing' }
RsrRemoteActionServer >> action: aBlock [

	action := aBlock
]

{ #category : 'debugging' }
RsrRemoteActionServer >> debug: anException
raisedDuring: aMessageSend
answerUsing: aResolver [

	^self debugHandler
		value: anException
		value: aMessageSend
		value: aResolver
]

{ #category : 'accessing' }
RsrRemoteActionServer >> debugHandler [

	^debugHandler ifNil: [[:x :y :z | nil]]
]

{ #category : 'accessing' }
RsrRemoteActionServer >> debugHandler: aBlock [

	debugHandler := aBlock
]

{ #category : 'evaluating' }
RsrRemoteActionServer >> value [

	^self action value
]

{ #category : 'evaluating' }
RsrRemoteActionServer >> value: anObject [

	^self action value: anObject
]

{ #category : 'evaluating' }
RsrRemoteActionServer >> valueWithArguments: anArray [

	^self action valueWithArguments: anArray
]
