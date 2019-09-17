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
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrWeakRegistryElement categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

!RsrObject class methodsFor!
new

	^super new initialize! !

!RsrObject methodsFor!
initialize

	^self! !


!RsrGarbageCollector class methodsFor!
maximumReclamation

	| service element sema |
	service := RsrAbstractService new.
	sema := Semaphore new.
	service toFinalizeEvaluate: (MessageSend receiver: sema selector: #signal).
	element := RsrWeakRegistryElement value: service.
	service := nil.
	MemoryManager current collectGarbage.
	[(Delay forMilliseconds: 10) wait.
	sema signal] forkAt: Processor systemBackgroundPriority.
	sema wait.
	^element value == DeadObject current! !

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
				selector: #removeKey:
				argument: aKey.
			aService toFinalizeEvaluate: finalizeSend.
			RsrWeakRegistryElement value: aService].
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
storageClass
	"Return the class of the storage"

	^Array!

value

	^storage at: 1!

value: anObject

	storage
		at: 1
		put: anObject!

initialize

	super initialize.
	storage := self storageClass new: 1! !

!RsrWeakRegistryElement methodsFor!
storageClass
	"Return the class of the storage"

	^WeakArray! !

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
	socket receive: bytes.
	^bytes!

socket: aHostSocket

	socket := aHostSocket!

write: aByteArray

	socket send: aByteArray! !
