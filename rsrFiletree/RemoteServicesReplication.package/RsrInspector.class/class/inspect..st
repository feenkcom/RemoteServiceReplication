actions
inspect: anObject
	| rsrInspectorClient |
	"If the user sent a String, then the object to be inspected lives on the other end of the connection."
	(anObject isKindOf: String)
		ifTrue: [ rsrInspectorClient := RsrInspectorClient new.
			rsrInspectorClient getObject: anObject ]
		ifFalse: [  ].
	^ rsrInspectorClient