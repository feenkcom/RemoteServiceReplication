Class {
	#name : 'RsrNonresumableError',
	#superclass : 'RsrError',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'testing' }
RsrNonresumableError >> isResumable [

	^false
]
