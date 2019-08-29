Class {
	#name : #RsrWeakRegistryElement,
	#superclass : #RsrRegistryElement,
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #accessing }
RsrWeakRegistryElement >> storageClass [
	"Return the class of the storage"

	^WeakArray
]
