| package |
package := Package name: 'RemoteServiceReplication-Base'.
package paxVersion: 1; basicComment: ''.

package classNames
	add: #RsrReference;
	add: #RsrIntegerReference;
	add: #RsrUnknownSID;
	add: #RsrCharacterReference;
	add: #RsrBrokenPromise;
	add: #RsrTrueReference;
	add: #RsrProcessModel;
	add: #RsrOrderedCollectionReference;
	add: #RsrInvalidBind;
	add: #RsrCharacterArrayReference;
	add: #RsrNonresumableError;
	add: #RsrImmediateReference;
	add: #RsrNegativeIntegerReference;
	add: #RsrUnsupportedObject;
	add: #RsrCollectionReference;
	add: #RsrResumableError;
	add: #RsrNilReference;
	add: #RsrError;
	add: #RsrObject;
	add: #RsrSetReference;
	add: #RsrSocketClosed;
	add: #RsrStringReference;
	add: #RsrPromiseError;
	add: #RsrBooleanReference;
	add: #RsrPositiveIntegerReference;
	add: #RsrWaitForConnectionCancelled;
	add: #RsrArrayReference;
	add: #RsrSocketError;
	add: #RsrValueReference;
	add: #RsrAlreadyRegistered;
	add: #RsrDateAndTime;
	add: #RsrDateAndTimeReference;
	add: #RsrUnknownClass;
	add: #RsrSymbolReference;
	add: #RsrAlreadyResolved;
	add: #RsrFalseReference;
	add: #RsrServiceReference;
	add: #RsrDictionaryReference;
	add: #RsrConnectFailed;
	add: #RsrConnectionFailed;
	add: #RsrByteArrayReference;
	add: #RsrConnectionClosed;
	yourself.

package methodNames
	yourself.

package setPrerequisites: #().

package!

Object
	subclass: #RsrObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrObject categoriesForClass!RemoteServiceReplication-Base! !

Object
	subclass: #RsrProcessModel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'current'!
!RsrProcessModel categoriesForClass!RemoteServiceReplication-Base! !

RsrObject
	subclass: #RsrDateAndTime
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrDateAndTime categoriesForClass!RemoteServiceReplication-Base! !

RsrObject
	subclass: #RsrReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'referenceMapping'!
RsrReference comment: 'RsrReferenceReference instances are created as a by-product of freezing the state of a Service. This typically happens when the framework creates a SendMessage or DeliverResponse command.The Reference represents and is able to resolve the object is it represents. In some cases, the value is immediate. In the case of ServiceReference, the stored Service Identifier is resolved in the context of a connection.Resolving must occur in the context of a Connection. Though this is true, the minimal information necessary for a Reference to resolve is the Registry.SendMessage and DeliverResponse store fields like receiver or result as references. They are resolved when the Command is set to execute.Collaborators:- ServiceSnapshot- Encoder- Decoder'!
!RsrReference categoriesForClass!RemoteServiceReplication-Base! !

Error
	subclass: #RsrError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrError categoriesForClass!RemoteServiceReplication-Base! !

RsrReference
	subclass: #RsrImmediateReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrImmediateReference comment: 'No class-specific documentation for RsrImmediateReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference( value)'!
!RsrImmediateReference categoriesForClass!RemoteServiceReplication-Base! !

RsrReference
	subclass: #RsrServiceReference
	instanceVariableNames: 'sid'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrServiceReference comment: 'No class-specific documentation for RsrServiceReference, hierarchy is:Object  RsrObject    RsrReference      RsrServiceReference( sid)'!
!RsrServiceReference categoriesForClass!RemoteServiceReplication-Base! !

RsrError
	subclass: #RsrAlreadyRegistered
	instanceVariableNames: 'service intendedConnection'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrAlreadyRegistered categoriesForClass!RemoteServiceReplication-Base! !

RsrImmediateReference
	subclass: #RsrBooleanReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrBooleanReference comment: 'No class-specific documentation for RsrBooleanReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrBooleanReference'!
!RsrBooleanReference categoriesForClass!RemoteServiceReplication-Base! !

RsrError
	subclass: #RsrConnectionClosed
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrConnectionClosed categoriesForClass!RemoteServiceReplication-Base! !

RsrError
	subclass: #RsrConnectionFailed
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrConnectionFailed categoriesForClass!RemoteServiceReplication-Base! !

RsrImmediateReference
	subclass: #RsrNilReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrNilReference comment: 'No class-specific documentation for RsrNilReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrNilReference'!
!RsrNilReference categoriesForClass!RemoteServiceReplication-Base! !

RsrError
	subclass: #RsrNonresumableError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrNonresumableError categoriesForClass!RemoteServiceReplication-Base! !

RsrError
	subclass: #RsrPromiseError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrPromiseError categoriesForClass!RemoteServiceReplication-Base! !

RsrError
	subclass: #RsrResumableError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrResumableError categoriesForClass!RemoteServiceReplication-Base! !

RsrError
	subclass: #RsrSocketError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrSocketError categoriesForClass!RemoteServiceReplication-Base! !

RsrError
	subclass: #RsrUnknownClass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrUnknownClass categoriesForClass!RemoteServiceReplication-Base! !

RsrError
	subclass: #RsrUnknownSID
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrUnknownSID categoriesForClass!RemoteServiceReplication-Base! !

RsrError
	subclass: #RsrUnsupportedObject
	instanceVariableNames: 'object'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrUnsupportedObject categoriesForClass!RemoteServiceReplication-Base! !

RsrImmediateReference
	subclass: #RsrValueReference
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrValueReference comment: 'No class-specific documentation for RsrValueReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrValueReference( value)'!
!RsrValueReference categoriesForClass!RemoteServiceReplication-Base! !

RsrError
	subclass: #RsrWaitForConnectionCancelled
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrWaitForConnectionCancelled categoriesForClass!RemoteServiceReplication-Base! !

RsrPromiseError
	subclass: #RsrAlreadyResolved
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrAlreadyResolved categoriesForClass!RemoteServiceReplication-Base! !

RsrPromiseError
	subclass: #RsrBrokenPromise
	instanceVariableNames: 'reason'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrBrokenPromise categoriesForClass!RemoteServiceReplication-Base! !

RsrValueReference
	subclass: #RsrByteArrayReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrByteArrayReference comment: 'No class-specific documentation for RsrByteArrayReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrValueReference( value)          RsrByteArrayReference'!
!RsrByteArrayReference categoriesForClass!RemoteServiceReplication-Base! !

RsrValueReference
	subclass: #RsrCharacterArrayReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrCharacterArrayReference comment: 'No class-specific documentation for RsrCharacterArrayReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrCharacterArrayReference'!
!RsrCharacterArrayReference categoriesForClass!RemoteServiceReplication-Base! !

RsrValueReference
	subclass: #RsrCharacterReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrCharacterReference comment: 'No class-specific documentation for RsrCharacterReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrValueReference( value)          RsrCharacterReference'!
!RsrCharacterReference categoriesForClass!RemoteServiceReplication-Base! !

RsrValueReference
	subclass: #RsrCollectionReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrCollectionReference comment: 'No class-specific documentation for RsrCollectionReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrValueReference( value)          RsrCollectionReference'!
!RsrCollectionReference categoriesForClass!RemoteServiceReplication-Base! !

RsrSocketError
	subclass: #RsrConnectFailed
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrConnectFailed categoriesForClass!RemoteServiceReplication-Base! !

RsrValueReference
	subclass: #RsrDateAndTimeReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrDateAndTimeReference comment: 'No class-specific documentation for RsrDateAndTimeReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrValueReference( value)          RsrDateAndTimeReference'!
!RsrDateAndTimeReference categoriesForClass!RemoteServiceReplication-Base! !

RsrBooleanReference
	subclass: #RsrFalseReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrFalseReference comment: 'No class-specific documentation for RsrFalseReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrFalseReference'!
!RsrFalseReference categoriesForClass!RemoteServiceReplication-Base! !

RsrValueReference
	subclass: #RsrIntegerReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrIntegerReference comment: 'No class-specific documentation for RsrIntegerReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrIntegerReference( value)'!
!RsrIntegerReference categoriesForClass!RemoteServiceReplication-Base! !

RsrSocketError
	subclass: #RsrInvalidBind
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrInvalidBind categoriesForClass!RemoteServiceReplication-Base! !

RsrSocketError
	subclass: #RsrSocketClosed
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrSocketClosed categoriesForClass!RemoteServiceReplication-Base! !

RsrBooleanReference
	subclass: #RsrTrueReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrTrueReference comment: 'No class-specific documentation for RsrTrueReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference( value)        RsrTrueReference'!
!RsrTrueReference categoriesForClass!RemoteServiceReplication-Base! !

RsrCollectionReference
	subclass: #RsrArrayReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrArrayReference comment: 'No class-specific documentation for RsrArrayReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrValueReference( value)          RsrArrayReference'!
!RsrArrayReference categoriesForClass!RemoteServiceReplication-Base! !

RsrCollectionReference
	subclass: #RsrDictionaryReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrDictionaryReference comment: 'No class-specific documentation for RsrDictionaryReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrValueReference( value)          RsrDictionaryReference'!
!RsrDictionaryReference categoriesForClass!RemoteServiceReplication-Base! !

RsrIntegerReference
	subclass: #RsrNegativeIntegerReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrNegativeIntegerReference comment: 'No class-specific documentation for RsrNegativeIntegerReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrIntegerReference( value)          RsrNegativeIntegerReference'!
!RsrNegativeIntegerReference categoriesForClass!RemoteServiceReplication-Base! !

RsrCollectionReference
	subclass: #RsrOrderedCollectionReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrOrderedCollectionReference comment: 'No class-specific documentation for RsrOrderedCollectionReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrValueReference( value)          RsrOrderedCollectionReference'!
!RsrOrderedCollectionReference categoriesForClass!RemoteServiceReplication-Base! !

RsrIntegerReference
	subclass: #RsrPositiveIntegerReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrPositiveIntegerReference comment: 'No class-specific documentation for RsrPositiveIntegerReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrIntegerReference( value)          RsrPositiveIntegerReference'!
!RsrPositiveIntegerReference categoriesForClass!RemoteServiceReplication-Base! !

RsrCollectionReference
	subclass: #RsrSetReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrSetReference comment: 'No class-specific documentation for RsrSetReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrValueReference( value)          RsrSetReference'!
!RsrSetReference categoriesForClass!RemoteServiceReplication-Base! !

RsrCharacterArrayReference
	subclass: #RsrStringReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrStringReference comment: 'No class-specific documentation for RsrStringReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrCharacterArrayReference( value)          RsrStringReference'!
!RsrStringReference categoriesForClass!RemoteServiceReplication-Base! !

RsrCharacterArrayReference
	subclass: #RsrSymbolReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RsrSymbolReference comment: 'No class-specific documentation for RsrSymbolReference, hierarchy is:Object  RsrObject    RsrReference      RsrImmediateReference        RsrCharacterArrayReference( value)          RsrSymbolReference'!
!RsrSymbolReference categoriesForClass!RemoteServiceReplication-Base! !

!RsrUnsupportedObject class methodsFor!
signal: anObject	^self new		object: anObject;		signal! !

!RsrOrderedCollectionReference class methodsFor!
typeIdentifier	^12! !

!RsrFalseReference class methodsFor!
typeIdentifier	^8! !

!RsrStringReference class methodsFor!
typeIdentifier	^2! !

!RsrBooleanReference class methodsFor!
from: aBoolean	^aBoolean		ifTrue: [RsrTrueReference new]		ifFalse: [RsrFalseReference new]! !

!RsrSymbolReference class methodsFor!
typeIdentifier	^1! !

!RsrSymbolReference class methodsFor!
symbol: aSymbol	^self new		value: aSymbol;		yourself! !

!RsrIntegerReference class methodsFor!
from: anInteger	^anInteger positive		ifTrue: [RsrPositiveIntegerReference value: anInteger]		ifFalse: [RsrNegativeIntegerReference value: anInteger]! !

!RsrCollectionReference class methodsFor!
analyze: aCollectionusing: anAnalyzer	^anAnalyzer analyzeCollection: aCollection! !

!RsrCollectionReference class methodsFor!
from: aSequencedCollection	| references |	references := (1 to: aSequencedCollection size) collect: [:i | RsrReference from: (aSequencedCollection at: i)].	^self value: references! !

!RsrBrokenPromise class methodsFor!
signalReason: aReason	^self new		reason: aReason;		signal! !

!RsrServiceReference class methodsFor!
sid: aServiceID	^self new		sid: aServiceID;		yourself! !

!RsrServiceReference class methodsFor!
analyze: aServiceusing: anAnalyzer	^anAnalyzer analyzeService: aService! !

!RsrServiceReference class methodsFor!
from: aService	^self sid: aService _id! !

!RsrReference class methodsFor!
referenceMapping	^referenceMapping ifNil: [self initializeReferenceMapping]! !

!RsrReference class methodsFor!
typeIdentifier	^self subclassResponsibility! !

!RsrReference class methodsFor!
analyze: anObjectusing: anAnalyzer	^self subclassResponsibility! !

!RsrReference class methodsFor!
from: anObject	| referenceClass |	referenceClass := self referenceClassFor: anObject.	^referenceClass from: anObject! !

!RsrNilReference class methodsFor!
from: aNil	^self new! !

!RsrDictionaryReference class methodsFor!
typeIdentifier	^13! !

!RsrDictionaryReference class methodsFor!
analyze: aDictionaryusing: anAnalyzer	^anAnalyzer analyzeDictionary: aDictionary! !

!RsrDictionaryReference class methodsFor!
from: aDictionary	| referenceStream |	referenceStream := WriteStream on: (Array new: aDictionary size * 2).	aDictionary		keysAndValuesDo:			[:key :value |			referenceStream				nextPut: (RsrReference from: key);				nextPut: (RsrReference from: value)].	^self value: referenceStream contents! !

!RsrDateAndTimeReference class methodsFor!
typeIdentifier	^14! !

!RsrArrayReference class methodsFor!
typeIdentifier	^9! !

!RsrProcessModel class methodsFor!
currentStackDump	^self current currentStackDump! !

!RsrProcessModel class methodsFor!
fork: aBlock	^self current fork: aBlock! !

!RsrProcessModel class methodsFor!
current	^current ifNil: [self resetCurrent]! !

!RsrProcessModel class methodsFor!
resetCurrent	^current := self new! !

!RsrProcessModel class methodsFor!
current: concurrency	current := concurrency! !

!RsrProcessModel class methodsFor!
fork: aBlockat: aPriority	^self current		fork: aBlock		at: aPriority! !

!RsrImmediateReference class methodsFor!
analyze: anObjectusing: anAnalyzer	^anAnalyzer analyzeImmediate: anObject! !

!RsrImmediateReference class methodsFor!
from: anObject	^self subclassResponsiblity! !

!RsrAlreadyRegistered class methodsFor!
signalService: aServiceintendedConnection: aConnection	^self new		service: aService;		intendedConnection: aConnection;		signal! !

!RsrValueReference class methodsFor!
value: anObject	^self new		value: anObject;		yourself! !

!RsrValueReference class methodsFor!
from: anObject	^self value: anObject! !

!RsrObject class methodsFor!
trace	Transcript		show: RsrProcessModel currentStackDump;		cr;		cr! !

!RsrTrueReference class methodsFor!
typeIdentifier	^7! !

!RsrSetReference class methodsFor!
typeIdentifier	^11! !

!RsrSetReference class methodsFor!
from: aSet	| referenceStream |	referenceStream := WriteStream on: (Array new: aSet size).	aSet do:  [:each | referenceStream nextPut: (RsrReference from: each)].	^self value: referenceStream contents! !

!RsrUnsupportedObject methodsFor!
object	^object! !

!RsrUnsupportedObject methodsFor!
object: anObject	object := anObject.	self messageText: 'Instances of ', object class name, ' cannot be serialized'! !

!RsrOrderedCollectionReference methodsFor!
resolve: aConnection	| oc |	oc := OrderedCollection new: value size.	value do: [:each | oc add: (each resolve: aConnection)].	^oc! !

!RsrPositiveIntegerReference methodsFor!
typeIdentifier	^3! !

!RsrConnectionClosed methodsFor!
messageText	^'The connection has closed'! !

!RsrFalseReference methodsFor!
resolve: aConnection	^false! !

!RsrBooleanReference methodsFor!
decode: aStreamusing: aDecoder	"Boolean has no additional value"! !

!RsrBooleanReference methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self typeIdentifier		onto: aStream! !

!RsrSymbolReference methodsFor!
convertBytes: aByteArray	^(super convertBytes: aByteArray) asSymbol! !

!RsrIntegerReference methodsFor!
convertBytes: aByteArray	^aByteArray		inject: 0		into: [:integer :byte | (integer bitShift: 8) bitOr: byte]! !

!RsrIntegerReference methodsFor!
decode: aStreamusing: aDecoder	| length bytes |	length := aDecoder decodeControlWord: aStream.	bytes := aStream next: length.	value := self convertBytes: bytes! !

!RsrIntegerReference methodsFor!
convertToBytes: anInteger	| stream int |	anInteger <= 0		ifTrue: [^#[0]].	stream := WriteStream on: (ByteArray new: 8).	int := anInteger.	[int > 0]		whileTrue:			[stream nextPut: (int bitAnd: 16rFF).			int := int bitShift: -8].	^stream contents reverse! !

!RsrIntegerReference methodsFor!
encode: aStreamusing: anEncoder	| bytes |	bytes := self convertToBytes: value abs.	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self typeIdentifier		onto: aStream.	anEncoder		encodeControlWord: bytes size		onto: aStream.	aStream nextPutAll: bytes! !

!RsrCollectionReference methodsFor!
decode: aStreamusing: aDecoder	| size |	size := aDecoder decodeControlWord: aStream.	value := (1 to: size) collect: [:i | aDecoder decodeReference: aStream]! !

!RsrCollectionReference methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self typeIdentifier		onto: aStream.	anEncoder		encodeControlWord: value size		onto: aStream.	value		do:			[:each |			each				encode: aStream				using: anEncoder]! !

!RsrNonresumableError methodsFor!
isResumable	^false! !

!RsrBrokenPromise methodsFor!
reason: aReason	reason := aReason! !

!RsrBrokenPromise methodsFor!
reason	^reason! !

!RsrByteArrayReference methodsFor!
typeIdentifier	^10! !

!RsrByteArrayReference methodsFor!
decode: aStreamusing: aDecoder	| length |	length := aDecoder decodeControlWord: aStream.	value := aStream next: length! !

!RsrByteArrayReference methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self typeIdentifier		onto: aStream.	anEncoder		encodeControlWord: value size		onto: aStream.	aStream nextPutAll: value! !

!RsrServiceReference methodsFor!
sid	^sid! !

!RsrServiceReference methodsFor!
sid: aServiceID	sid := aServiceID! !

!RsrServiceReference methodsFor!
resolve: aConnection	^aConnection serviceAt: self sid! !

!RsrServiceReference methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeControlWord: self sid		onto: aStream! !

!RsrReference methodsFor!
typeIdentifier	^self class typeIdentifier! !

!RsrReference methodsFor!
resolve: aConnection	"Resolve the reference in the context of the provided Connection."	^self subclassResponsibility! !

!RsrNilReference methodsFor!
typeIdentifier	^6! !

!RsrNilReference methodsFor!
decode: aStreamusing: aDecoder	"Nil has no additional value"! !

!RsrNilReference methodsFor!
resolve: aConnection	^nil! !

!RsrNilReference methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self typeIdentifier		onto: aStream! !

!RsrCharacterArrayReference methodsFor!
decode: aStreamusing: aDecoder	| length bytes |	length := aDecoder decodeControlWord: aStream.	bytes := aStream next: length.	value := self convertBytes: bytes! !

!RsrCharacterArrayReference methodsFor!
encode: aStreamusing: anEncoder	| bytes |	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self typeIdentifier		onto: aStream.	bytes := self convertToBytes: value.	anEncoder		encodeControlWord: bytes size		onto: aStream.	aStream nextPutAll: bytes! !

!RsrNegativeIntegerReference methodsFor!
convertBytes: aByteArray	^(super convertBytes: aByteArray) negated! !

!RsrNegativeIntegerReference methodsFor!
typeIdentifier	^4! !

!RsrDictionaryReference methodsFor!
decode: aStreamusing: aDecoder	| size |	size := aDecoder decodeControlWord: aStream.	value := (1 to: size * 2) collect: [:each | aDecoder decodeReference: aStream]! !

!RsrDictionaryReference methodsFor!
resolve: aConnection	| stream numEntries dictionary |	stream := ReadStream on: value.	numEntries := value size / 2.	dictionary := Dictionary new: numEntries.	numEntries		timesRepeat:			[dictionary				at: (stream next resolve: aConnection)				put: (stream next resolve: aConnection)].	^dictionary! !

!RsrDictionaryReference methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self typeIdentifier		onto: aStream.	anEncoder		encodeControlWord: value size / 2		onto: aStream.	value do: [:each | each encode: aStream using: anEncoder]! !

!RsrDateAndTimeReference methodsFor!
decode: aStreamusing: aDecoder	| microseconds |	microseconds := aDecoder decodeControlWord: aStream.	value := RsrDateAndTime fromMicroseconds: microseconds! !

!RsrDateAndTimeReference methodsFor!
encode: aStreamusing: anEncoder	| microseconds |	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self typeIdentifier		onto: aStream.	microseconds := RsrDateAndTime microsecondsSinceEpoch: value.	anEncoder		encodeControlWord: microseconds		onto: aStream! !

!RsrArrayReference methodsFor!
resolve: aConnection	^value collect: [:each | each resolve: aConnection]! !

!RsrImmediateReference methodsFor!
immediateOID	^0! !

!RsrCharacterReference methodsFor!
typeIdentifier	^5! !

!RsrCharacterReference methodsFor!
decode: aStreamusing: aDecoder	| codePoint |	codePoint := aDecoder decodeControlWord: aStream.	value := Character codePoint: codePoint! !

!RsrCharacterReference methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self typeIdentifier		onto: aStream.	anEncoder		encodeControlWord: value codePoint		onto: aStream! !

!RsrProcessModel methodsFor!
fork: aBlockat: aPriority	^aBlock forkAt: aPriority! !

!RsrProcessModel methodsFor!
fork: aBlock	^aBlock fork! !

!RsrAlreadyRegistered methodsFor!
service	^service! !

!RsrAlreadyRegistered methodsFor!
service: aService	service := aService! !

!RsrAlreadyRegistered methodsFor!
intendedConnection	^intendedConnection! !

!RsrAlreadyRegistered methodsFor!
intendedConnection: aConnection	intendedConnection := aConnection! !

!RsrValueReference methodsFor!
resolve: aConnection	^value! !

!RsrValueReference methodsFor!
value: anObject	value := anObject! !

!RsrResumableError methodsFor!
isResumable	^true! !

!RsrObject methodsFor!
note: aString	"This method can be used to leave a note in code. For instance, a code path that needs to be tested."! !

!RsrObject methodsFor!
trace	Transcript		show: RsrProcessModel currentStackDump;		cr;		cr! !

!RsrObject methodsFor!
minimalWait	"Ensure the calling process is not schedulable for a short period of time."	(Delay forMilliseconds: 1) wait! !

!RsrSetReference methodsFor!
decode: aStreamusing: aDecoder	| size |	size := aDecoder decodeControlWord: aStream.	value :=  (1 to: size) collect: [:i | aDecoder decodeReference: aStream]! !

!RsrSetReference methodsFor!
resolve: aConnection	| set |	set := Set new.	value do: [:each | set add: (each resolve: aConnection)].	^set! !

!RsrSetReference methodsFor!
encode: aStreamusing: anEncoder	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self typeIdentifier		onto: aStream.	anEncoder		encodeControlWord: value size		onto: aStream.	value		do:			[:each |			each				encode: aStream				using: anEncoder]! !

!RsrTrueReference methodsFor!
resolve: aConnection	^true! !