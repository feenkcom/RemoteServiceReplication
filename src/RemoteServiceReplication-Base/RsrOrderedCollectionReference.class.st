"
No class-specific documentation for RsrOrderedCollectionReference, hierarchy is:
Object
  RsrObject
    RsrReference
      RsrImmediateReference
        RsrValueReference( value)
          RsrOrderedCollectionReference

"
Class {
	#name : 'RsrOrderedCollectionReference',
	#superclass : 'RsrCollectionReference',
	#category : 'RemoteServiceReplication-Base'
}

{ #category : 'accessing' }
RsrOrderedCollectionReference class >> typeIdentifier [

	^12
]

{ #category : 'resolving' }
RsrOrderedCollectionReference >> resolve: aRegistry [

	| oc |
	oc := OrderedCollection new: value size.
	value do: [:each | oc add: (each resolve: aRegistry)].
	^oc
]
