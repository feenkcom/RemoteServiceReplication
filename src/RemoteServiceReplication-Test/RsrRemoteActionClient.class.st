Class {
	#name : 'RsrRemoteActionClient',
	#superclass : 'RsrRemoteAction',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'evaluating' }
RsrRemoteActionClient >> value [

	^remoteSelf value wait
]

{ #category : 'evaluating' }
RsrRemoteActionClient >> value: anObject [

	^(remoteSelf value: anObject) wait
]

{ #category : 'evaluating' }
RsrRemoteActionClient >> valueWithArguments: anArray [

	^(remoteSelf valueWithArguments: anArray) wait
]
