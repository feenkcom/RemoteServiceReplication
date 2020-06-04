Class {
	#name : 'RsrWeakRegistryEntry',
	#superclass : 'RsrRegistryEntry',
	#instVars : [
		'mournAction'
	],
	#category : 'RemoteServiceReplication-GemStone'
}

{ #category : 'instance-creation' }
RsrWeakRegistryEntry class >> with: anObject
mournAction: aBlock [

	^(self with: anObject)
        mournAction: aBlock;
        beEphemeron: true;
        yourself
]

{ #category : 'mourning' }
RsrWeakRegistryEntry >> mourn [

	mournAction value.
    service := mournAction := nil
]

{ #category : 'accessing' }
RsrWeakRegistryEntry >> mournAction: aBlock [

	mournAction := aBlock
]
