private-encoding
reflectedVariablesOf: anRsrObject
do: aBlock

	anRsrObject class variablesToReflect
		do:
			[:name |
			aBlock value: (anRsrObject instVarNamed: name)]