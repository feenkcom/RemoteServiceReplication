Class {
	#name : #RsrEnvironment,
	#superclass : #RsrObject,
	#category : #'RemoteServiceReplication-Compatibility-Pharo-RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #branching }
RsrEnvironment class >> ifPharo: aPharoBlock
ifGemStone: aGemStoneBlock [

	^aPharoBlock value
]
