Extension { #name : #Array }

{ #category : #'*TonelFixes' }
Array >> asTonelValue [
	^ self collect: [ :each | each asTonelValue ]
]
