Class {
	#name : 'RsrEnvironment',
	#superclass : 'RsrObject',
	#category : 'RemoteServiceReplication-Compatibility-GemStone'
}

{ #category : 'branching' }
RsrEnvironment class >> ifPharo: aPharoBlock
ifGemStone: aGemStoneBlock
ifDolphin: aDolphinBlock [

	^aGemStoneBlock value
]
