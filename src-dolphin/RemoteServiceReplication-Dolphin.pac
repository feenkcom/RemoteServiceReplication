| package |
package := Package name: 'RemoteServiceReplication-Dolphin'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #RsrAbstractService;
	add: #RsrClassResolver;
	add: #RsrEnvironment;
	add: #RsrForwarder;
	add: #RsrGarbageCollector;
	add: #RsrObject;
	add: #RsrProtoObject;
	add: #RsrRegistry;
	add: #RsrRegistryEntry;
	add: #RsrScientist;
	add: #RsrSocket;
	add: #RsrWeakRegistryEntry;
	yourself.

package methodNames
	add: #Object -> #asString;
	add: #RsrProcessModel -> #currentStackDump;
	add: #SequenceableCollection -> #doWithIndex:;
	add: #Set -> #hash;
	add: 'RsrCharacterArraySpecies class' -> #fromBytes:;
	add: 'RsrCharacterArraySpecies class' -> #toBytes:;
	add: 'RsrDateAndTimeSpecies class' -> #fromMicroseconds:;
	add: 'RsrDateAndTimeSpecies class' -> #microsecondsSinceEpoch:;
	add: 'RsrDateAndTimeSpecies class' -> #now;
	add: 'RsrDateAndTimeSpecies class' -> #posixEpoch;
	add: 'RsrSpecies class' -> #initializeSpeciesMapping;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #('RemoteServiceReplication-Base').

package!

"Class Definitions"!

Object subclass: #RsrObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrObject subclass: #RsrAbstractService
	instanceVariableNames: 'finalizationSend'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrObject subclass: #RsrClassResolver
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrObject subclass: #RsrEnvironment
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrObject subclass: #RsrGarbageCollector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrObject subclass: #RsrRegistry
	instanceVariableNames: 'mutex map reapAction'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrObject subclass: #RsrRegistryEntry
	instanceVariableNames: 'storage dispatcher'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrObject subclass: #RsrScientist
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrObject subclass: #RsrSocket
	instanceVariableNames: 'socket'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrRegistryEntry subclass: #RsrWeakRegistryEntry
	instanceVariableNames: 'finalizationSend'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ProtoObject subclass: #RsrProtoObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrProtoObject subclass: #RsrForwarder
	instanceVariableNames: '_service'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Object methodsFor!

asString

	^self printString! !
!Object categoriesFor: #asString!converting!public! !

!RsrCharacterArraySpecies class methodsFor!

fromBytes: aByteArray

	^Utf8String fromByteArray: aByteArray!

toBytes: aCharacterArray

	^aCharacterArray asUtf8String asByteArray! !
!RsrCharacterArraySpecies class categoriesFor: #fromBytes:!public! !
!RsrCharacterArraySpecies class categoriesFor: #toBytes:!public! !

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
!RsrDateAndTimeSpecies class categoriesFor: #fromMicroseconds:!public! !
!RsrDateAndTimeSpecies class categoriesFor: #microsecondsSinceEpoch:!public! !
!RsrDateAndTimeSpecies class categoriesFor: #now!public! !
!RsrDateAndTimeSpecies class categoriesFor: #posixEpoch!public! !

!RsrProcessModel methodsFor!

currentStackDump

	^Processor activeProcess stackTrace: 1000! !
!RsrProcessModel categoriesFor: #currentStackDump!public! !

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
		at: AnsiString
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
!RsrSpecies class categoriesFor: #initializeSpeciesMapping!public! !

!SequenceableCollection methodsFor!

doWithIndex: aBlock

	| index size |
	index := 1.
	size := self size.
	[index <= size]
		whileTrue:
			[aBlock
				value: (self at: index)
				value: index.
			index := index + 1]! !
!SequenceableCollection categoriesFor: #doWithIndex:!public! !

!Set methodsFor!

hash

	^self
		inject: #Set hash
		into: [:hash :each | hash bitXor: each hash]! !
!Set categoriesFor: #hash!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

RsrObject guid: (GUID fromString: '{8faf60e5-9213-4b3a-b8ca-d6e46209c662}')!
RsrObject comment: ''!
!RsrObject categoriesForClass!RemoteServiceReplication-Dolphin! !
!RsrObject methodsFor!

initialize

	^self! !
!RsrObject categoriesFor: #initialize!public! !

!RsrObject class methodsFor!

new

	^super new initialize! !
!RsrObject class categoriesFor: #new!public! !

RsrAbstractService guid: (GUID fromString: '{7fe58ad6-2d95-4e01-9243-9cb9d5506477}')!
RsrAbstractService comment: ''!
!RsrAbstractService categoriesForClass!Unclassified! !
!RsrAbstractService methodsFor!

finalize

	finalizationSend value!

toFinalizeEvaluate: aMessageSend

	finalizationSend := aMessageSend.
	self beFinalizable! !
!RsrAbstractService categoriesFor: #finalize!public! !
!RsrAbstractService categoriesFor: #toFinalizeEvaluate:!public! !

RsrClassResolver guid: (GUID fromString: '{bcecf4c2-9299-4982-b23b-1f72cc6e0c96}')!
RsrClassResolver comment: ''!
!RsrClassResolver categoriesForClass!RemoteServiceReplication-Dolphin! !
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
!RsrClassResolver class categoriesFor: #classNamed:!public! !
!RsrClassResolver class categoriesFor: #classNamed:ifAbsent:!public! !

RsrEnvironment guid: (GUID fromString: '{d34e5cc4-a2e0-49ec-8483-17b8faccc9f0}')!
RsrEnvironment comment: ''!
!RsrEnvironment categoriesForClass!RemoteServiceReplication-Dolphin! !
!RsrEnvironment class methodsFor!

ifPharo: p
ifGemStone: g
ifDolphin: aBlock

	^aBlock value! !
!RsrEnvironment class categoriesFor: #ifPharo:ifGemStone:ifDolphin:!public! !

RsrGarbageCollector guid: (GUID fromString: '{6b2d74f4-36e4-45a4-95c8-e886a1855d81}')!
RsrGarbageCollector comment: ''!
!RsrGarbageCollector categoriesForClass!RemoteServiceReplication-Dolphin! !
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
	element := RsrWeakRegistryEntry value: service toFinalizeEvaluate: action.
	service := nil.
	self invokeGarbageCollector.
	
	[(Delay forSeconds: 1) wait.	"Wait up to one second for finalization"
	sema signal] fork.
	sema wait.
	^didFinalize! !
!RsrGarbageCollector class categoriesFor: #invokeGarbageCollector!public! !
!RsrGarbageCollector class categoriesFor: #maximumReclamation!public! !

RsrRegistry guid: (GUID fromString: '{5192e31f-c2f6-463e-b545-31a8190ee6be}')!
RsrRegistry comment: 'I maintain the associations between locally stored objects and their remote counterparts.'!
!RsrRegistry categoriesForClass!RemoteServiceReplication-Dolphin! !
!RsrRegistry methodsFor!

at: aKey put: anEntry
	"Store anEntry into the registry"

	mutex critical: [map at: aKey put: anEntry]!

cleanupEntryFor: aKey

	self removeKey: aKey!

dispatcherAt: aKey

	^self
		dispatcherAt: aKey
		ifAbsent: [Error signal: 'Unknown key(', aKey asString, ')']!

dispatcherAt: aKey
ifAbsent: aBlock

	| entry |
	entry := mutex critical: [map at: aKey ifAbsent: []].
	^entry
		ifNil: aBlock
		ifNotNil: [entry dispatcher]!

elementValue: anElement
ifNil: aBlock

	| value |
	anElement isNil
		ifTrue: [^aBlock value].
	value := anElement service.
	^value == DeadObject current
		ifTrue: [aBlock value]
		ifFalse: [value]!

includesKey: aKey

	^mutex critical: [map includesKey: aKey]!

initialize

	super initialize.
	map := Dictionary new.
	mutex := Semaphore forMutualExclusion!

reap: aKey

	self cleanupEntryFor: aKey.
	self reapAction value: aKey!

reapAction

	^reapAction!

reapAction: aBlock

	reapAction := aBlock!

removeKey: aKey

	^mutex critical: [map removeKey: aKey ifAbsent: [nil]]!

serviceAt: aKey

	^self serviceAt: aKey ifAbsent: [Error signal: 'Unknown key: ', aKey asString]!

serviceAt: aKey
ifAbsent: aBlock

	| element |
	element := mutex critical: [map at: aKey ifAbsent: []].
	^self
		elementValue: element
		ifNil: aBlock!

serviceAt: aKey
put: aService
	"Store aService into the registry"

	| entry |
	entry := aService isServer
				ifTrue: [RsrRegistryEntry value: aService]
				ifFalse: 
					[| finalizeSend |
					finalizeSend := MessageSend
								receiver: self
								selector: #reap:
								argument: aKey.
					RsrWeakRegistryEntry value: aService toFinalizeEvaluate: finalizeSend].
	self
		at: aKey
		put: entry.
	^aService! !
!RsrRegistry categoriesFor: #at:put:!public! !
!RsrRegistry categoriesFor: #cleanupEntryFor:!public! !
!RsrRegistry categoriesFor: #dispatcherAt:!public! !
!RsrRegistry categoriesFor: #dispatcherAt:ifAbsent:!public! !
!RsrRegistry categoriesFor: #elementValue:ifNil:!public! !
!RsrRegistry categoriesFor: #includesKey:!public! !
!RsrRegistry categoriesFor: #initialize!public! !
!RsrRegistry categoriesFor: #reap:!public! !
!RsrRegistry categoriesFor: #reapAction!public! !
!RsrRegistry categoriesFor: #reapAction:!public! !
!RsrRegistry categoriesFor: #removeKey:!public! !
!RsrRegistry categoriesFor: #serviceAt:!public! !
!RsrRegistry categoriesFor: #serviceAt:ifAbsent:!public! !
!RsrRegistry categoriesFor: #serviceAt:put:!public! !

!RsrRegistry class methodsFor!

new

	^self reapAction: [:key | ]!

reapAction: aBlock

	^super new
		reapAction: aBlock;
		yourself! !
!RsrRegistry class categoriesFor: #new!public! !
!RsrRegistry class categoriesFor: #reapAction:!public! !

RsrRegistryEntry guid: (GUID fromString: '{636f9a23-234c-4a7a-ab6d-6d1700364857}')!
RsrRegistryEntry comment: ''!
!RsrRegistryEntry categoriesForClass!RemoteServiceReplication-Dolphin! !
!RsrRegistryEntry methodsFor!

dispatcher

	^dispatcher!

dispatcher: aMessageDispatcher

	dispatcher := aMessageDispatcher!

initialize

	super initialize.
	self initializeStorage!

initializeStorage

	storage := Array new: 1!

service

	^storage at: 1!

service: aService

	storage
		at: 1
		put: aService!

value

	^storage at: 1!

value: anObject

	storage
		at: 1
		put: anObject! !
!RsrRegistryEntry categoriesFor: #dispatcher!public! !
!RsrRegistryEntry categoriesFor: #dispatcher:!public! !
!RsrRegistryEntry categoriesFor: #initialize!public! !
!RsrRegistryEntry categoriesFor: #initializeStorage!public! !
!RsrRegistryEntry categoriesFor: #service!public! !
!RsrRegistryEntry categoriesFor: #service:!public! !
!RsrRegistryEntry categoriesFor: #value!public! !
!RsrRegistryEntry categoriesFor: #value:!public! !

!RsrRegistryEntry class methodsFor!

value: anObject

	^self new
		value: anObject;
		yourself! !
!RsrRegistryEntry class categoriesFor: #value:!public! !

RsrScientist guid: (GUID fromString: '{ca700baf-795f-44da-aef0-cc7d2ad19d68}')!
RsrScientist comment: ''!
!RsrScientist categoriesForClass!RemoteServiceReplication-Dolphin! !
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
!RsrScientist categoriesFor: #instrument:label:!public! !
!RsrScientist categoriesFor: #profile:label:!public! !
!RsrScientist categoriesFor: #profile:lable:if:!public! !

RsrSocket guid: (GUID fromString: '{401a4593-5b85-47ad-9860-48941a585902}')!
RsrSocket comment: ''!
!RsrSocket categoriesForClass!RemoteServiceReplication-Dolphin! !
!RsrSocket methodsFor!

accept

	^self class on: socket accept!

close

	socket close!

connectToHost: aHostname
port: aPort

	socket := Socket2
		port: aPort
		host: aHostname.
	socket connect!

dataAvailable

	^socket notNil and: [socket hasInput]!

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
		do: [:ex | socket close.  ex resignalAs: RsrSocketClosed new].
	^bytes!

readAvailable

	| bytes totalBytesRead bytesRead |
	bytes := ByteArray new: 4096.
	totalBytesRead := 0.
	[self dataAvailable]
		whileTrue:
			[bytes size = totalBytesRead
				ifTrue: [bytes := bytes, (ByteArray new: 4096)].
			bytesRead := socket
				receiveSome: bytes
				count: bytes size - totalBytesRead
				startingAt: totalBytesRead + 1.
			totalBytesRead := totalBytesRead + bytesRead].
	^bytes copyFrom: 1 to: totalBytesRead!

socket: aHostSocket

	socket := aHostSocket!

write: aByteArray

	socket send: aByteArray! !
!RsrSocket categoriesFor: #accept!public! !
!RsrSocket categoriesFor: #close!public! !
!RsrSocket categoriesFor: #connectToHost:port:!public! !
!RsrSocket categoriesFor: #dataAvailable!public! !
!RsrSocket categoriesFor: #isConnected!public! !
!RsrSocket categoriesFor: #listenOn:!public! !
!RsrSocket categoriesFor: #read:!public! !
!RsrSocket categoriesFor: #readAvailable!public! !
!RsrSocket categoriesFor: #socket:!public! !
!RsrSocket categoriesFor: #write:!public! !

!RsrSocket class methodsFor!

on: aHostSocket

	^self new
		socket: aHostSocket;
		yourself! !
!RsrSocket class categoriesFor: #on:!public! !

RsrWeakRegistryEntry guid: (GUID fromString: '{551e557a-1b07-488a-a72a-92a9aee28fdf}')!
RsrWeakRegistryEntry comment: ''!
!RsrWeakRegistryEntry categoriesForClass!RemoteServiceReplication-Dolphin! !
!RsrWeakRegistryEntry methodsFor!

elementsExpired: anInteger
of: anArray

	finalizationSend ifNotNil: [:action | [action value] fork]!

initializeStorage

	storage := MourningWeakArray new: 1.
	storage pathologist: self!

toFinalizeEvaluate: anEvaluable

	finalizationSend := anEvaluable! !
!RsrWeakRegistryEntry categoriesFor: #elementsExpired:of:!public! !
!RsrWeakRegistryEntry categoriesFor: #initializeStorage!public! !
!RsrWeakRegistryEntry categoriesFor: #toFinalizeEvaluate:!public! !

!RsrWeakRegistryEntry class methodsFor!

value: aService
toFinalizeEvaluate: anEvaluable

	^(self value: aService)
		toFinalizeEvaluate: anEvaluable;
		yourself! !
!RsrWeakRegistryEntry class categoriesFor: #value:toFinalizeEvaluate:!public! !

RsrProtoObject guid: (GUID fromString: '{8c807a21-ab7c-4f1d-9c7a-f012b4953ad7}')!
RsrProtoObject comment: ''!
!RsrProtoObject categoriesForClass!RemoteServiceReplication-Dolphin! !
RsrForwarder guid: (GUID fromString: '{63d391a4-77f2-42aa-82a9-31cb9e9ed808}')!
RsrForwarder comment: ''!
!RsrForwarder categoriesForClass!Unclassified! !
!RsrForwarder methodsFor!

aspectDisplayOn: aStream

	aStream
		nextPutAll: 'RsrForwarder(';
		print: _service;
		nextPutAll: ')'!

class

	^RsrForwarder!

icon

	^self class icon!

newAspect: each
class: aspectClass

	^aspectClass name: each!

respondsTo: aSelector

	^self class canUnderstand: aSelector! !
!RsrForwarder categoriesFor: #aspectDisplayOn:!private! !
!RsrForwarder categoriesFor: #class!private! !
!RsrForwarder categoriesFor: #icon!private! !
!RsrForwarder categoriesFor: #newAspect:class:!private! !
!RsrForwarder categoriesFor: #respondsTo:!private! !

"Binary Globals"!

