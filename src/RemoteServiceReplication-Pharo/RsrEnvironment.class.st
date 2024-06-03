Class {
	#name : #RsrEnvironment,
	#superclass : #RsrObject,
	#category : 'RemoteServiceReplication-Pharo'
}

{ #category : #branching }
RsrEnvironment class >> ifPharo: aPharoBlock
ifGemStone: aGemStoneBlock
ifDolphin: aDolphinBlock [

	^aPharoBlock value
]
