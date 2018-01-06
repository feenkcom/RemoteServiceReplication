receiving
processDeserializedObject: anObject
	"This method receives an object over the wire and processes it based on it's type."
	(anObject isKindOf: RsrGcProxy) 
		ifTrue: [ |rr|
			rr := RsrRegistry uniqueInstance.
			rr cleanup.
		].
	
	(anObject isKindOf: RsrEvaluateOnServer) 
		ifTrue: [ ^anObject execute ].
		
	(anObject isKindOf: RsrReference ) 
		ifTrue: [ ^anObject].
		
	^nil
	
