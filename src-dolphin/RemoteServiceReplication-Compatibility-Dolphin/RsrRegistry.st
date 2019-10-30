RsrObject
	subclass: #RsrRegistry
	instanceVariableNames: 'mutex map reapAction'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrRegistry comment: 'I maintain the associations between locally stored objects and their remote counterparts.'!
!RsrRegistry categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

RsrObject
	subclass: #RsrRegistryElement
	instanceVariableNames: 'storage'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrRegistryElement categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

RsrRegistryElement
	subclass: #RsrWeakRegistryElement
	instanceVariableNames: 'finalizationSend'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrWeakRegistryElement categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !


!RsrRegistry class methodsFor!
new

	^self reapAction: [:key | ]!

reapAction: aBlock

	^super new
		reapAction: aBlock;
		yourself! !

!RsrRegistry methodsFor!
elementValue: anElement
ifNil: aBlock

	| value |
	anElement isNil
		ifTrue: [^aBlock value].
	value := anElement value.
	^value == DeadObject current
		ifTrue: [aBlock value]
		ifFalse: [value]!

at: aKey

	^self at: aKey ifAbsent: [Error signal: 'Unknown key: ', aKey asString]!

at: aKey
ifAbsent: aBlock

	| element |
	element := mutex critical: [map at: aKey ifAbsent: []].
	^self
		elementValue: element
		ifNil: aBlock!

at: aKey
put: aService
	"Store aService into the registry"

	| element |
	element := aService isServer
		ifTrue: [RsrRegistryElement value: aService]
		ifFalse:
			[| finalizeSend |
			finalizeSend := MessageSend
				receiver: self
				selector: #reap:
				argument: aKey.
			RsrWeakRegistryElement
				value: aService
				toFinalizeEvaluate: finalizeSend].
	mutex critical: [map at: aKey put: element]!

removeKey: aKey

	| element value |
	element := mutex critical: [map removeKey: aKey ifAbsent: [nil]].
	^self
		elementValue: element
		ifNil: [nil]!

includesKey: aKey

	^mutex critical: [map includesKey: aKey]!

initialize

	super initialize.
	map := Dictionary new.
	mutex := Semaphore forMutualExclusion!

reapAction

	^reapAction!

reapAction: aBlock

	reapAction := aBlock!

reap: aKey

	self removeKey: aKey.
	self reapAction value: aKey! !

!RsrRegistryElement class methodsFor!
value: anObject

	^self new
		value: anObject;
		yourself! !

!RsrRegistryElement methodsFor!
initializeStorage

	storage := Array new: 1!

value

	^storage at: 1!

value: anObject

	storage
		at: 1
		put: anObject!

initialize

	super initialize.
	self initializeStorage! !

!RsrWeakRegistryElement class methodsFor!
value: aService
toFinalizeEvaluate: anEvaluable

	^(self value: aService)
		toFinalizeEvaluate: anEvaluable;
		yourself! !

!RsrWeakRegistryElement methodsFor!
initializeStorage

	storage := MourningWeakArray new: 1.
	storage pathologist: self!

toFinalizeEvaluate: anEvaluable

	finalizationSend := anEvaluable!

elementsExpired: anInteger
of: anArray

	finalizationSend ifNotNil: [:action | [action value] fork]! !
