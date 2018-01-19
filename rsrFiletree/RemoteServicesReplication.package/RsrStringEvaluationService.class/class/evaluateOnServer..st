convenience
evaluateOnServer: anExpression
	|rsrStringEvaluationService|
	rsrStringEvaluationService := RsrStringEvaluationService new.
	^rsrStringEvaluationService evaluateOnServer: anExpression.