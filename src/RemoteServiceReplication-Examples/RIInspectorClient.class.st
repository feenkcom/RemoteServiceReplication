Class {
	#name : #RIInspectorClient,
	#superclass : #RIAbstractInspector,
	#category : 'RemoteServiceReplication-Examples-Inspector'
}

{ #category : #'inspection protocol' }
RIInspectorClient >> objectPrintString [

	^remoteSelf objectPrintString
]
