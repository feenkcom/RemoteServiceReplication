Class {
	#name : #RsrScientist,
	#superclass : #RsrObject,
	#category : #'RemoteServiceReplication-Compatibility-GemStone'
}

{ #category : #instrumenting }
RsrScientist >> instrument: aBlock
label: aString [

	^aBlock value
]

{ #category : #instrumenting }
RsrScientist >> profile: aBlock
label: aString [

	^aBlock value
]

{ #category : #instrumenting }
RsrScientist >> profile: aBlock
label: aString
if: aCondition [

	^aBlock value
]
