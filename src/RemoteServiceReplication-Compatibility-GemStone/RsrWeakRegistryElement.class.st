Class {
	#name : #RsrWeakRegistryElement,
	#superclass : #RsrRegistryElement,
	#instVars : [
		'mournAction'
	],
	#category : #'RemoteServiceReplication-Compatibility-GemStone'
}

{ #category : #'instance-creation' }
RsrWeakRegistryElement class >> with: anObject
mournAction: aBlock [

	^(self with: anObject)
        mournAction: aBlock;
        beEphemeron: true;
        yourself
]

{ #category : #accessing }
RsrWeakRegistryElement >> mournAction: aBlock [

	mournAction := aBlock
]

{ #category : #mourning }
RsrWeakRegistryElement >> mourn [

	mournAction value.
    value := mournAction := nil
]