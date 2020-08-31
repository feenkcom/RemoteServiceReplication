"
No class-specific documentation for RsrStringReference, hierarchy is:
Object
  RsrObject
    RsrReference
      RsrImmediateReference
        RsrCharacterArrayReference( value)
          RsrStringReference

"
Class {
	#name : 'RsrStringReference',
	#superclass : 'RsrCharacterArrayReference',
	#category : 'RemoteServiceReplication'
}

{ #category : 'accessing' }
RsrStringReference class >> typeIdentifier [

	^2
]
