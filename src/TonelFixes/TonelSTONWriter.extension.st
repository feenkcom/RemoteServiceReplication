Extension { #name : 'TonelSTONWriter' }

{ #category : '*TonelFixes' }
TonelSTONWriter >> encodeKey: key value: value [
	super encodeKey: key value: value asTonelValue
]
