Class {
	#name : 'RsrEnvironment',
	#superclass : 'RsrObject',
	#category : 'RemoteServiceReplication-GemStone'
}

{ #category : 'branching' }
RsrEnvironment class >> ifPharo: aPharoBlock
ifGemStone: aGemStoneBlock
ifDolphin: aDolphinBlock [

	^aGemStoneBlock value
]
