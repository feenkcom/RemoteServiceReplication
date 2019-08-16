run
Object
	subclass: #RsrMockService
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
TestCase
	subclass: #RsrTestCase
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrGarbageCollectorTestCase
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrRegistryTest
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrClassResolverTestCase
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

run
RsrTestCase
	subclass: #RsrSocketTest
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%





set class RsrMockService

method:
rsrId	^1
%

set class RsrTestCase class

classmethod:
isAbstract	^self == RsrTestCase
%

set class RsrTestCase

method:
hack: aString	"Placeholder for things that need to be fixed"
%



set class RsrTestCase

method:
defaultTimeLimit	^5 seconds
%



set class RsrTestCase

method:
assert: anObjectidenticalTo: bObject	self assert: anObject == bObject
%



set class RsrTestCase

method:
deny: anObjectidenticalTo: bObject	self assert: anObject ~~ bObject
%



set class RsrTestCase

method:
maximumReclamation	self assert: RsrGarbageCollector maximumReclamation
%

set class RsrGarbageCollectorTestCase

method:
testMaximumReclamation	self assert: RsrGarbageCollector maximumReclamation
%

set class RsrRegistryTest

method:
testRegister	| id object registry marker |	marker := Object new.	object := RsrMockService new.	id := object rsrId.	registry := RsrRegistry new.	registry register: object.	self maximumReclamation.	self		assert: (registry at: id ifAbsent: [marker])		identicalTo: object.	object := nil.	self maximumReclamation.	self		assert: (registry at: id ifAbsent: [marker])		identicalTo: marker
%



set class RsrRegistryTest

method:
testRetain	| id object registry marker |	marker := Object new.	object := RsrMockService new.	id := object rsrId.	registry := RsrRegistry new.	registry retain: object.	object := nil.	self maximumReclamation.	object := registry at: id ifAbsent: [marker].	self		deny: object		equals: marker.	self		assert: object class		equals: RsrMockService.	self		assert: object rsrId		equals: id
%

set class RsrClassResolverTestCase

method:
assert: aClassNameresolvesTo: expectedClass	| actualClass |	actualClass := RsrClassResolver classNamed: aClassName.	self		assert: actualClass		identicalTo: expectedClass
%



set class RsrClassResolverTestCase

method:
testSuccessfulResolution	| actual |	actual := RsrClassResolver classNamed: #Object.	self		assert: actual		identicalTo: Object.	actual := RsrClassResolver		classNamed: #Object		ifAbsent: [self assert: false].	self		assert: actual		identicalTo: Object
%



set class RsrClassResolverTestCase

method:
testFailedResolution	| actual marker |	self		should: [RsrClassResolver classNamed: #Xlerb]		raise: Error.	marker := Object new.	actual := RsrClassResolver		classNamed: #Xlerb		ifAbsent: [marker].	self		assert: actual		identicalTo: marker
%

set class RsrSocketTest

method:
testConnectToInvalidPort	self assert: false
%