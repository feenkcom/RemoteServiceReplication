private-encoding
reflectedVariablesOf: anRsrObject
do: aBlock

	(self reflectedVariableNamesOf: anRsrObject)
		do:
			[:name |
			aBlock value: (anRsrObject instVarNamed: name)]