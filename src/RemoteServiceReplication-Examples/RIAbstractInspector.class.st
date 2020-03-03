Class {
	#name : #RIAbstractInspector,
	#superclass : #RsrService,
	#category : #'RemoteServiceReplication-Examples-Inspector'
}

{ #category : #accessing }
RIAbstractInspector class >> clientClassName [

	^#RIInspectorClient
]

{ #category : #accessing }
RIAbstractInspector class >> serverClassName [

	^#RIInspectorServer
]
