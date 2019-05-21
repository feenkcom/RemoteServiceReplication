private-encoding
reflectedVariableNamesOf: anRsrObject
do: aBlock

	anRsrObject class variablesToReflect
		do:
			[:name |
			aBlock value: name]