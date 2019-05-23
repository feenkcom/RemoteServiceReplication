private-testing
isImmediate: anObject

	^anObject isSymbol
		or: [anObject isString
			or: [anObject isInteger
				or: [anObject isCharacter
					or: [#(nil true false) includes: anObject]]]]