"
No class-specific documentation for RsrImmediateReference, hierarchy is:
Object
  RsrObject
    RsrReference
      RsrImmediateReference( value)

"
Class {
	#name : 'RsrImmediateReference',
	#superclass : 'RsrReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrImmediateReference class >> value: anObject [

	^self new
		value: anObject;
		yourself
]

{ #category : 'resolving' }
RsrImmediateReference >> resolve: aRegistry [

	^self subclassResponsibility
]
