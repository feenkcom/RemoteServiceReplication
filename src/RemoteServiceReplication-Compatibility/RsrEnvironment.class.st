Class {
	#name : #RsrEnvironment,
	#superclass : #RsrObject,
	#category : #'RemoteServiceReplication-Compatibility'
}

{ #category : #branching }
RsrEnvironment class >> ifPharo: aPharoBlock
ifGemStone: aGemStoneBlock [

	^aPharoBlock value
]
