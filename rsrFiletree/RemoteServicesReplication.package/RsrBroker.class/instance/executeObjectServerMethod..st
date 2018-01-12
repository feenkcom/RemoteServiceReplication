receiving
executeObjectServerMethod: anObject
	"This method receives an object over the wire and processes it based on it's type."
	
	"This should be refactored to receive an object of a known type and behave appropriately."
	(anObject isKindOf: RsrStringEvaluationService) 
		ifTrue: [ ^anObject execute ].
		
	(anObject isKindOf: RsrReference ) 
		ifTrue: [ ^anObject].
		
	^nil
	
