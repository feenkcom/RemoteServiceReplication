| package |
package := Package name: 'RemoteServiceReplication-Base'.
package paxVersion: 1; basicComment: ''.

package classNames
	add: #RsrSocketClosed;
	add: #RsrByteArraySpecies;
	add: #RsrPositiveIntegerSpecies;
	add: #RsrTrueSpecies;
	add: #RsrNegativeIntegerSpecies;
	add: #RsrWaitForConnectionCancelled;
	add: #RsrFalseSpecies;
	add: #RsrIntegerSpecies;
	add: #RsrConnectionClosed;
	add: #RsrBooleanSpecies;
	add: #RsrDictionarySpecies;
	add: #RsrError;
	add: #RsrArraySpecies;
	add: #RsrDateAndTimeSpecies;
	add: #RsrUndefinedObjectSpecies;
	add: #RsrSpecies;
	add: #RsrCharacterSpecies;
	add: #RsrSetSpecies;
	add: #RsrProcessModel;
	add: #RsrAlreadyRegistered;
	add: #RsrInvalidBind;
	add: #RsrSymbolSpecies;
	add: #RsrServiceSpecies;
	add: #RsrUnsupportedObject;
	add: #RsrStringSpecies;
	add: #RsrConnectFailed;
	add: #RsrOrderedCollectionSpecies;
	add: #RsrUnknownClass;
	add: #RsrCharacterArraySpecies;
	add: #RsrSocketError;
	add: #RsrNullSpecies;
	yourself.

package methodNames
	yourself.

package setPrerequisites: #().

package!

Object
	subclass: #RsrProcessModel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'current'!
!RsrProcessModel categoriesForClass!RemoteServiceReplication-Base! !

Object
	subclass: #RsrSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'speciesMapping'!
!RsrSpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrSpecies
	subclass: #RsrArraySpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrArraySpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrSpecies
	subclass: #RsrBooleanSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrBooleanSpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrSpecies
	subclass: #RsrByteArraySpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrByteArraySpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrSpecies
	subclass: #RsrCharacterArraySpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrCharacterArraySpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrSpecies
	subclass: #RsrCharacterSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrCharacterSpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrSpecies
	subclass: #RsrDateAndTimeSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrDateAndTimeSpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrSpecies
	subclass: #RsrDictionarySpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrDictionarySpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrSpecies
	subclass: #RsrIntegerSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrIntegerSpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrSpecies
	subclass: #RsrNullSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrNullSpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrSpecies
	subclass: #RsrOrderedCollectionSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrOrderedCollectionSpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrSpecies
	subclass: #RsrServiceSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrServiceSpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrSpecies
	subclass: #RsrSetSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrSetSpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrSpecies
	subclass: #RsrUndefinedObjectSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrUndefinedObjectSpecies categoriesForClass!RemoteServiceReplication-Base! !

Error
	subclass: #RsrError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrError categoriesForClass!RemoteServiceReplication-Base! !

RsrBooleanSpecies
	subclass: #RsrFalseSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrFalseSpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrIntegerSpecies
	subclass: #RsrNegativeIntegerSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrNegativeIntegerSpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrIntegerSpecies
	subclass: #RsrPositiveIntegerSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrPositiveIntegerSpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrCharacterArraySpecies
	subclass: #RsrStringSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrStringSpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrCharacterArraySpecies
	subclass: #RsrSymbolSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrSymbolSpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrBooleanSpecies
	subclass: #RsrTrueSpecies
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrTrueSpecies categoriesForClass!RemoteServiceReplication-Base! !

RsrError
	subclass: #RsrAlreadyRegistered
	instanceVariableNames: 'service intendedConnection'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrAlreadyRegistered categoriesForClass!RemoteServiceReplication-Base! !

RsrError
	subclass: #RsrConnectionClosed
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrConnectionClosed categoriesForClass!RemoteServiceReplication-Base! !

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
	subclass: #RsrUnsupportedObject
	instanceVariableNames: 'object'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrUnsupportedObject categoriesForClass!RemoteServiceReplication-Base! !

RsrError
	subclass: #RsrWaitForConnectionCancelled
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrWaitForConnectionCancelled categoriesForClass!RemoteServiceReplication-Base! !

RsrSocketError
	subclass: #RsrConnectFailed
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrConnectFailed categoriesForClass!RemoteServiceReplication-Base! !

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

!RsrServiceSpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	self shouldNotImplement! !

!RsrServiceSpecies class methodsFor!
speciesIdentifier	^0! !

!RsrServiceSpecies class methodsFor!
decode: aStreamusing: aDecoder	"Decode a Service from the stream"	| species oid instVarCount serviceName instance |	species := aDecoder decodeControlWord: aStream.	oid := aDecoder decodeControlWord: aStream.	instVarCount := aDecoder decodeControlWord: aStream.	serviceName := aDecoder decodeAndResolveObjectReference: aStream.	instance := aDecoder registry		serviceAt: oid		ifAbsent:			[((aDecoder lookupClass: serviceName)				_id: oid				connection: aDecoder connection)					yourself].	(aDecoder registry includesKey: oid)		ifFalse:			[aDecoder registry				serviceAt: instance _id				put: instance].	(self reflectedVariablesFor: instance) size = instVarCount		ifFalse: [self error: 'Incorrectly encoded instance detected'].	self		reflectedVariableIndicesFor: instance		do: [:index | instance instVarAt: index put: (aDecoder decodeAndResolveObjectReference: aStream)].	^instance! !

!RsrServiceSpecies class methodsFor!
encodeReference: aServiceusing: anEncoderonto: aStream	anEncoder		encodeControlWord: aService _id		onto: aStream! !

!RsrServiceSpecies class methodsFor!
analyze: aServiceusing: anAnalyzer	"A method that works in conjunction with RsrRetainAnalysis to analyze	an object"	^anAnalyzer analyzeService: aService! !

!RsrServiceSpecies class methodsFor!
encode: aServiceusing: anEncoderon: aStream	"Encode this object. This is specifically used by RsrServiceSpecies."	"type"	"the OID for the object"	"the name of the remote service to create"	"Write the object slots"	| reflectedVariables remoteServiceName |	reflectedVariables := self reflectedVariablesFor: aService.	anEncoder		encodeControlWord: self speciesIdentifier		onto: aStream.	anEncoder		encodeControlWord: aService _id		onto: aStream.	anEncoder		encodeControlWord: reflectedVariables size		onto: aStream.	remoteServiceName := aService isClient		ifTrue: [aService class serverClassName]		ifFalse: [aService class clientClassName].	(anEncoder speciesOf: remoteServiceName)		encodeReference: remoteServiceName		using: anEncoder		onto: aStream.	RsrServiceSpecies		reflectedVariablesFor: aService		do: [:each | anEncoder encodeReferenceOf: each onto: aStream]! !

!RsrStringSpecies class methodsFor!
speciesIdentifier	^2! !

!RsrUnsupportedObject class methodsFor!
signal: anObject	^self new		object: anObject;		signal! !

!RsrDictionarySpecies class methodsFor!
encodeReference: aDictionaryusing: anEncoderonto: aStream	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self speciesIdentifier		onto: aStream.	anEncoder		encodeControlWord: aDictionary size		onto: aStream.	aDictionary		keysAndValuesDo:			[:key :value |			anEncoder				encodeReferenceOf: key				onto: aStream.			anEncoder				encodeReferenceOf: value				onto: aStream]! !

!RsrDictionarySpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	| size dictionary |	size := aDecoder decodeControlWord: aStream.	dictionary := Dictionary new: size.	1		to: size		do:			[:i |			dictionary				at: (aDecoder decodeAndResolveObjectReference: aStream)				put: (aDecoder decodeAndResolveObjectReference: aStream)].	^dictionary! !

!RsrDictionarySpecies class methodsFor!
speciesIdentifier	^13! !

!RsrDictionarySpecies class methodsFor!
analyze: aDictionaryusing: anAnalyzer	"A method that works in conjunction with RsrRetainAnalysis to analyze	an object"	^anAnalyzer analyzeDictionary: aDictionary! !

!RsrNullSpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	self shouldNotImplement! !

!RsrNullSpecies class methodsFor!
speciesIdentifier	^self shouldNotImplement! !

!RsrNullSpecies class methodsFor!
encodeReference: anObjectusing: anEncoderonto: aStream	^RsrUnsupportedObject signal: anObject! !

!RsrNullSpecies class methodsFor!
encode: anObjectusing: anEncoderon: aStream	"Encode this object. This is specifically used by RsrServiceSpecies."	RsrUnsupportedObject signal: anObject! !

!RsrNullSpecies class methodsFor!
analyze: anObjectusing: anAnalyzer	"A method that works in conjunction with RsrRetainAnalysis to analyze	an object"	^RsrUnsupportedObject signal: anObject! !

!RsrIntegerSpecies class methodsFor!
encodeReference: anIntegerusing: anEncoderonto: aStream	| bytes |	bytes := self integerAsByteArray: anInteger abs.	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self speciesIdentifier		onto: aStream.	anEncoder		encodeControlWord: bytes size		onto: aStream.	aStream nextPutAll: bytes! !

!RsrIntegerSpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	| length bytes |	length := aDecoder decodeControlWord: aStream.	bytes := aStream next: length.	^aDecoder bytesAsInteger: bytes! !

!RsrIntegerSpecies class methodsFor!
speciesIdentifier	^self subclassResponsibility! !

!RsrIntegerSpecies class methodsFor!
integerAsByteArray: anInteger	"Return a ByteArray representing <anInteger> in big endian format."	| stream int |	anInteger <= 0		ifTrue: [^ByteArray with: 0].	stream := WriteStream on: (ByteArray new: 8).	int := anInteger.	[int > 0]		whileTrue:			[stream nextPut: (int bitAnd: 16rFF).			int := int bitShift: -8].	^stream contents reverse! !

!RsrOrderedCollectionSpecies class methodsFor!
encodeReference: anOrderedCollectionusing: anEncoderonto: aStream	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self speciesIdentifier		onto: aStream.	anEncoder		encodeControlWord: anOrderedCollection size		onto: aStream.	anOrderedCollection		do:			[:each |			anEncoder				encodeReferenceOf: each				onto: aStream]! !

!RsrOrderedCollectionSpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	| size oc |	size := aDecoder decodeControlWord: aStream.	oc := OrderedCollection new: size.	size timesRepeat: [oc add: (aDecoder decodeAndResolveObjectReference: aStream)].	^oc! !

!RsrOrderedCollectionSpecies class methodsFor!
speciesIdentifier	^12! !

!RsrOrderedCollectionSpecies class methodsFor!
analyze: anOrderedCollectionusing: anAnalyzer	"A method that works in conjunction with RsrRetainAnalysis to analyze	an object"	^anAnalyzer analyzeCollection: anOrderedCollection! !

!RsrCharacterArraySpecies class methodsFor!
encodeReference: aCharacterArrayusing: anEncoderonto: aStream	| bytes |	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self speciesIdentifier		onto: aStream.	bytes := self toBytes: aCharacterArray.	anEncoder		encodeControlWord: bytes size		onto: aStream.	aStream nextPutAll: bytes! !

!RsrCharacterArraySpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	| length bytes |	length := aDecoder decodeControlWord: aStream.	bytes := aStream next: length.	^self fromBytes: bytes! !

!RsrCharacterArraySpecies class methodsFor!
speciesIdentifier	^self subclassResponsibility! !

!RsrByteArraySpecies class methodsFor!
encodeReference: aByteArrayusing: anEncoderonto: aStream	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self speciesIdentifier		onto: aStream.	anEncoder		encodeControlWord: aByteArray size		onto: aStream.	aStream nextPutAll: aByteArray! !

!RsrByteArraySpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	| size |	size := aDecoder decodeControlWord: aStream.	^aStream next: size! !

!RsrByteArraySpecies class methodsFor!
speciesIdentifier	^10! !

!RsrSetSpecies class methodsFor!
encodeReference: aSetusing: anEncoderonto: aStream	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self speciesIdentifier		onto: aStream.	anEncoder		encodeControlWord: aSet size		onto: aStream.	aSet		do:			[:each |			anEncoder				encodeReferenceOf: each				onto: aStream]! !

!RsrSetSpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	| size set |	size := aDecoder decodeControlWord: aStream.	set := Set new: size.	size timesRepeat: [set add: (aDecoder decodeAndResolveObjectReference: aStream)].	^set! !

!RsrSetSpecies class methodsFor!
speciesIdentifier	^11! !

!RsrSetSpecies class methodsFor!
analyze: aSetusing: anAnalyzer	"A method that works in conjunction with RsrRetainAnalysis to analyze	an object"	^anAnalyzer analyzeCollection: aSet! !

!RsrArraySpecies class methodsFor!
encodeReference: anArrayusing: anEncoderonto: aStream	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self speciesIdentifier		onto: aStream.	anEncoder		encodeControlWord: anArray size		onto: aStream.	anArray		do:			[:each |			anEncoder				encodeReferenceOf: each				onto: aStream]! !

!RsrArraySpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	| size array |	size := aDecoder decodeControlWord: aStream.	array := Array new: size.	1 to: size do: [:i | array at: i put: (aDecoder decodeAndResolveObjectReference: aStream)].	^array! !

!RsrArraySpecies class methodsFor!
speciesIdentifier	^9! !

!RsrArraySpecies class methodsFor!
analyze: anArrayusing: anAnalyzer	"A method that works in conjunction with RsrRetainAnalysis to analyze	an object"	^anAnalyzer analyzeCollection: anArray! !

!RsrSymbolSpecies class methodsFor!
decodeReference: aStream using: aDecoder	"Decode the provided bytes into the default native class for this species"	^ (super decodeReference: aStream using: aDecoder) asSymbol! !

!RsrSymbolSpecies class methodsFor!
speciesIdentifier	^1! !

!RsrDateAndTimeSpecies class methodsFor!
encodeReference: aDateAndTimeusing: anEncoderonto: aStream	| microseconds |	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self speciesIdentifier		onto: aStream.	microseconds := self microsecondsSinceEpoch: aDateAndTime.	anEncoder		encodeControlWord: microseconds		onto: aStream! !

!RsrDateAndTimeSpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	| microseconds |	microseconds := aDecoder decodeControlWord: aStream.	^self fromMicroseconds: microseconds! !

!RsrDateAndTimeSpecies class methodsFor!
speciesIdentifier	^14! !

!RsrSpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	self subclassResponsibility! !

!RsrSpecies class methodsFor!
speciesMapping	"Return a mapping between the native class and their associated RsrSpecies"	^speciesMapping ifNil: [self initializeSpeciesMapping]! !

!RsrSpecies class methodsFor!
speciesIdentifier	^self subclassResponsibility! !

!RsrSpecies class methodsFor!
encodeReference: anObjectusing: anEncoderonto: aStream	"Encode the native object using the provided encoder"	self subclassResponsibility! !

!RsrSpecies class methodsFor!
encode: anObjectusing: anEncoderon: aStream	"Encode this object. This is specifically used by RsrServiceSpecies."	self shouldNotImplement! !

!RsrSpecies class methodsFor!
analyze: anObjectusing: anAnalyzer	"A method that works in conjunction with RsrRetainAnalysis to analyze	an object"	^anAnalyzer analyzeImmediate: anObject! !

!RsrSpecies class methodsFor!
nullSpecies	^RsrNullSpecies! !

!RsrSpecies class methodsFor!
speciesList	^{RsrServiceSpecies.	RsrSymbolSpecies.	RsrStringSpecies.	RsrPositiveIntegerSpecies.	RsrNegativeIntegerSpecies.	RsrCharacterSpecies.	RsrUndefinedObjectSpecies.	RsrTrueSpecies.	RsrFalseSpecies.	RsrArraySpecies.	RsrByteArraySpecies.	RsrSetSpecies.	RsrOrderedCollectionSpecies.	RsrDictionarySpecies.	RsrDateAndTimeSpecies.}! !

!RsrFalseSpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	^false! !

!RsrFalseSpecies class methodsFor!
speciesIdentifier	^8! !

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

!RsrBooleanSpecies class methodsFor!
encodeReference: aBooleanusing: anEncoderonto: aStream	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self speciesIdentifier		onto: aStream! !

!RsrBooleanSpecies class methodsFor!
speciesIdentifier	^self subclassResponsibility! !

!RsrTrueSpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	^true! !

!RsrTrueSpecies class methodsFor!
speciesIdentifier	^7! !

!RsrPositiveIntegerSpecies class methodsFor!
speciesIdentifier	^3! !

!RsrAlreadyRegistered class methodsFor!
signalService: aServiceintendedConnection: aConnection	^self new		service: aService;		intendedConnection: aConnection;		signal! !

!RsrNegativeIntegerSpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	^(super decodeReference: aStream using: aDecoder) negated! !

!RsrNegativeIntegerSpecies class methodsFor!
speciesIdentifier	^4! !

!RsrCharacterSpecies class methodsFor!
encodeReference: aCharacterusing: anEncoderonto: aStream	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self speciesIdentifier		onto: aStream.	anEncoder		encodeControlWord: aCharacter codePoint		onto: aStream! !

!RsrCharacterSpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	| codePoint |	codePoint := aDecoder decodeControlWord: aStream.	^Character codePoint: codePoint! !

!RsrCharacterSpecies class methodsFor!
speciesIdentifier	^5! !

!RsrUndefinedObjectSpecies class methodsFor!
encodeReference: nilArgumentusing: anEncoderonto: aStream	anEncoder		encodeControlWord: anEncoder immediateOID		onto: aStream.	anEncoder		encodeControlWord: self speciesIdentifier		onto: aStream! !

!RsrUndefinedObjectSpecies class methodsFor!
decodeReference: aStreamusing: aDecoder	"Decode the provided bytes into the default native class for this species"	^nil! !

!RsrUndefinedObjectSpecies class methodsFor!
speciesIdentifier	^6! !

!RsrProcessModel methodsFor!
fork: aBlockat: aPriority	^aBlock forkAt: aPriority! !

!RsrProcessModel methodsFor!
fork: aBlock	^aBlock fork! !

!RsrUnsupportedObject methodsFor!
messageText	^'Instances of ', object class name, ' cannot be serialized'! !

!RsrUnsupportedObject methodsFor!
object	^object! !

!RsrUnsupportedObject methodsFor!
object: anObject	object := anObject! !

!RsrAlreadyRegistered methodsFor!
service	^service! !

!RsrAlreadyRegistered methodsFor!
service: aService	service := aService! !

!RsrAlreadyRegistered methodsFor!
intendedConnection	^intendedConnection! !

!RsrAlreadyRegistered methodsFor!
intendedConnection: aConnection	intendedConnection := aConnection! !

!RsrConnectionClosed methodsFor!
messageText	^'The connection has closed'! !