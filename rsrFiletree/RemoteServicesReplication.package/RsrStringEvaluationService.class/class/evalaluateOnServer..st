convenience
evalaluateOnServer: anExpression
	|rsrStringEvaluationService|
	rsrStringEvaluationService := RsrStringEvaluationService new.
	^rsrStringEvaluationService evalaluateOnServer: anExpression.