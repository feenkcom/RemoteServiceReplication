Class {
	#name : #RsrWeakRegistryEntry,
	#superclass : #RsrRegistryEntry,
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #accessing }
RsrWeakRegistryEntry >> storageClass [
	"Return the class of the storage"

	^WeakArray
]
