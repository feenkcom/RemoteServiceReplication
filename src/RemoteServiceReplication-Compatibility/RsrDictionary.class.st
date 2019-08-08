"
This class is intended to emulate a Dictionary containing strong keys and weak values which are finalized.

When a value stored in the dictionary and is not referenced elsewhere, the object should be finalized.
"
Class {
	#name : #RsrDictionary,
	#superclass : #WeakValueDictionary,
	#category : #'RemoteServiceReplication-Compatibility'
}
