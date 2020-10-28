Class {
	#name : 'RsrRemoteActionClient',
	#superclass : 'RsrRemoteAction',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'evaluating' }
RsrRemoteActionClient >> asyncValue [

	^remoteSelf value
]

{ #category : 'evaluating' }
RsrRemoteActionClient >> asyncValue: anObject [

	^remoteSelf value: anObject
]

{ #category : 'evaluating' }
RsrRemoteActionClient >> value [

	^self asyncValue wait
]

{ #category : 'evaluating' }
RsrRemoteActionClient >> value: anObject [

	^(self asyncValue: anObject) wait
]
