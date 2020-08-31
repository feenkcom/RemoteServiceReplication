"
No class-specific documentation for RsrNegativeIntegerReference, hierarchy is:
Object
  RsrObject
    RsrReference
      RsrImmediateReference
        RsrIntegerReference( value)
          RsrNegativeIntegerReference

"
Class {
	#name : 'RsrNegativeIntegerReference',
	#superclass : 'RsrIntegerReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'converting' }
RsrNegativeIntegerReference >> convertBytes: aByteArray [

	^(super convertBytes: aByteArray) negated
]

{ #category : 'accessing' }
RsrNegativeIntegerReference >> typeIdentifier [

	^4
]
