Class {
	#name : #RIInspectorClient,
	#superclass : #RIAbstractInspector,
	#category : #'RemoteServiceReplication-Examples-Inspector'
}

{ #category : #'glamorous-inspector' }
RIInspectorClient >> gtInspectorVariableNodesIn: aCollection [

	super gtInspectorVariableNodesIn: aCollection.
	aCollection add: (GTInspectorDynamicNode hostObject: self label: 'object' value: self objectPrintString)
]

{ #category : #'glamorous-inspector' }
RIInspectorClient >> gtInspectorVariableValuePairs [

	^"super gtInspectorVariableValuePairs"
	OrderedCollection new
		add: ('object' -> remoteSelf objectPrintString);
		yourself
]

{ #category : #'inspection protocol' }
RIInspectorClient >> objectPrintString [

	^remoteSelf objectPrintString
]
