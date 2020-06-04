Class {
	#name : #RsrWeakRegistryEntry,
	#superclass : #RsrRegistryEntry,
	#category : #'RemoteServiceReplication-Pharo'
}

{ #category : #accessing }
RsrWeakRegistryEntry >> storageClass [
	"Return the class of the storage"

	^WeakArray
]
