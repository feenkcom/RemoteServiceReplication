processing
isDataObject: anObject

	^anObject == true
		or: [anObject == false
			or: [anObject == nil
				or: [anObject isString
					or: [anObject isSymbol
						or: [anObject isInteger]]]]]