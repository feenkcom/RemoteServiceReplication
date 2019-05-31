Class {
	#name : #RIAbstractInspector,
	#superclass : #RsrService,
	#category : 'RemoteServiceReplication-Examples-Inspector'
}

{ #category : #accessing }
RIAbstractInspector class >> clientClass [

	^RIInspectorClient
]

{ #category : #accessing }
RIAbstractInspector class >> serverClass [

	^RIInspectorServer
]
