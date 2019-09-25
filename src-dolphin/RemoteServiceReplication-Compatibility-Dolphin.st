ProtoObject
	subclass: #RsrProtoObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrProtoObject categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

Object
	subclass: #RsrObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrObject categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

RsrObject
	subclass: #RsrAbstractService
	instanceVariableNames: 'finalizationSend'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrClassResolver categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

RsrObject
	subclass: #RsrClassResolver
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrClassResolver categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

RsrObject
	subclass: #RsrDateAndTimeInterface
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrDateAndTimeInterface categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

RsrObject
	subclass: #RsrEnvironment
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrEnvironment categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

RsrObject
	subclass: #RsrGarbageCollector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrGarbageCollector categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

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

RsrObject
	subclass: #RsrScientist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrScientist categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

RsrObject
	subclass: #RsrSocket
	instanceVariableNames: 'socket'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrSocket categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

RsrRegistryElement
	subclass: #RsrWeakRegistryElement
	instanceVariableNames: 'finalizationSend'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrWeakRegistryElement categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !


!RsrGarbageCollector class methodsFor!
invokeGarbageCollector

	MemoryManager current collectGarbage!

maximumReclamation

	| service element sema didFinalize action |
	service := RsrAbstractService new.
	sema := Semaphore new.
	didFinalize := false.
	action :=
		[didFinalize := true.
		sema signal].
	element := RsrWeakRegistryElement
		value: service
		toFinalizeEvaluate: action.
	service := nil.
	self invokeGarbageCollector.
	[(Delay forSeconds: 1) wait. "Wait up to one second for finalization"
	sema signal] fork.
	sema wait.
	^didFinalize! !

!RsrScientist methodsFor!
instrument: aBlock
label: aString

	^aBlock value!

profile: aBlock
label: aString

	^aBlock value!

profile: aBlock
lable: aString
if: aCondition

	^aBlock value! !

!RsrClassResolver class methodsFor!
classNamed: aSymbol

	^self
		classNamed: aSymbol
		ifAbsent: [RsrUnknownClass signal: aSymbol]!

classNamed: aSymbol
ifAbsent: aBlock

	^Smalltalk
		at: aSymbol
		ifAbsent: aBlock! !



!RsrAbstractService methodsFor!
toFinalizeEvaluate: aMessageSend

	finalizationSend := aMessageSend.
	self beFinalizable!

finalize

	finalizationSend value! !



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

!RsrSocket class methodsFor!
on: aHostSocket

	^self new
		socket: aHostSocket;
		yourself! !

!RsrSocket methodsFor!
accept

	^self class on: socket accept!

close

	socket close!

connectTo: aPort
on: aHostname

	socket := Socket2
		port: aPort
		host: aHostname.
	socket connect!

isConnected

	^socket notNil and: [socket isOpen]!

listenOn: aPort

	socket := ServerSocket2
		port: aPort
		backlog: 1!

read: aCount

	| bytes |
	bytes := ByteArray new: aCount.
	[socket receive: bytes]
		on: SocketClosed
		do: [:ex | socket close.  ex resignalAs: RsrConnectionClosed new].
	^bytes!

socket: aHostSocket

	socket := aHostSocket!

write: aByteArray

	socket send: aByteArray! !


!RsrEnvironment class methodsFor!
ifPharo: p
ifGemStone: g
ifDolphin: aBlock

	^aBlock value! !



!RsrObject class methodsFor!
new

	^super new initialize! !

!RsrObject methodsFor!
initialize

	^self! !


!RsrDateAndTimeInterface class methodsFor!
dateAndTimeClass

	^TimeStamp!

fromString: aString

	^self dateAndTimeClass current! !

!RsrSpecies class methodsFor!
initializeSpeciesMapping

	speciesMapping := Dictionary new.
	speciesMapping
		at: Symbol
		put: RsrSymbolSpecies.
	speciesMapping
		at: String
		put: RsrStringSpecies.
	speciesMapping
		at: Utf8String
		put: RsrStringSpecies.
	speciesMapping
		at: LargeInteger
		put: RsrIntegerSpecies.
	speciesMapping
		at: SmallInteger
		put: RsrIntegerSpecies.
	speciesMapping
		at: Character
		put: RsrCharacterSpecies.
	speciesMapping
		at: UndefinedObject
		put: RsrUndefinedObjectSpecies.
	speciesMapping
		at: True
		put: RsrBooleanSpecies.
	speciesMapping
		at: False
		put: RsrBooleanSpecies.
	speciesMapping
		at: Array
		put: RsrArraySpecies.
	speciesMapping
		at: ByteArray
		put: RsrByteArraySpecies.
	speciesMapping
		at: Set
		put: RsrSetSpecies.
	speciesMapping
		at: OrderedCollection
		put: RsrOrderedCollectionSpecies.
	speciesMapping
		at: Dictionary
		put: RsrDictionarySpecies.
	speciesMapping
		at: TimeStamp
		put: RsrDateAndTimeSpecies.
	^speciesMapping! !


!RsrCharacterArraySpecies class methodsFor!
fromBytes: aByteArray

	^Utf8String fromByteArray: aByteArray!

toBytes: aCharacterArray

	^aCharacterArray asUtf8String asByteArray! !


!RsrDateAndTimeSpecies class methodsFor!
fromMicroseconds: anInteger

	^TimeStamp fromMilliseconds: self posixEpoch asMilliseconds + (anInteger // 1000)!

microsecondsSinceEpoch: aTimeStamp

	| millisDiff |
	millisDiff := aTimeStamp asMilliseconds - self posixEpoch asMilliseconds.
	^millisDiff * 1000!

now

	^TimeStamp current!

posixEpoch

	^TimeStamp fromSeconds: 2177452800! !