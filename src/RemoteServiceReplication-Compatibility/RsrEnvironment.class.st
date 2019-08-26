Class {
	#name : #RsrEnvironment,
	#superclass : #ProtoObject,
	#category : #'RemoteServiceReplication-Compatibility'
}

{ #category : #branching }
RsrEnvironment class >> ifPharo: aPharoBlock
ifGemStone: aGemStoneBlock [

	^aPharoBlock value
]
