Class {
	#name : 'RsrRemoteActionClient',
	#superclass : 'RsrRemoteAction',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'evaluating' }
RsrRemoteActionClient >> value [

	^remoteSelf value
]

{ #category : 'evaluating' }
RsrRemoteActionClient >> value: anObject [

	^remoteSelf value: anObject
]

{ #category : 'evaluating' }
RsrRemoteActionClient >> valueWithArguments: anArray [

	^remoteSelf valueWithArguments: anArray
]
