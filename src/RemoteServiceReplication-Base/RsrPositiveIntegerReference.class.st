"
No class-specific documentation for RsrPositiveIntegerReference, hierarchy is:
Object
  RsrObject
    RsrReference
      RsrImmediateReference
        RsrIntegerReference( value)
          RsrPositiveIntegerReference

"
Class {
	#name : 'RsrPositiveIntegerReference',
	#superclass : 'RsrIntegerReference',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'accessing' }
RsrPositiveIntegerReference class >> typeIdentifier [

	^3
]
