run
RsrObject
	subclass: #RsrRegistry
	instVarNames: #(mutex map storage reapAction)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

set class RsrRegistry

classmethod:
new

	^self reapAction: [:x |]
%

classmethod:
reapAction: aBlock

	^super new
		reapAction: aBlock;
		yourself
%

method:
initialize

	mutex := Semaphore forMutualExclusion.
	map := Dictionary new.
	storage := IdentitySet new
%

method:
reapAction: aBlock

	reapAction := aBlock
%

method:
critical: aBlock

	mutex wait.
	^aBlock ensure: [mutex signal]
%

method:
removeKey: anId

	| element |
	element := self
		critical:
			[map
				removeKey: anId
				ifAbsent: [nil]].
	^element ifNotNil: [element key]
%

method:
register: aService

	| element |
	element := RsrEphemeron
		on: aService
		mournAction:
			[self removeKey: aService rsrId.
			reapAction value: aService rsrId].
	self critical: [map at: aService rsrId put: element]
%

method:
retain: aService

	| element |
	element := RsrEphemeron
		on: aService
		mournAction: [:x |].
	self
		critical:
			[map at: aService rsrId put: element.
			storage add: aService]
%

method:
at: aKey

	^self
		at: aKey
		ifAbsent: [self error: 'Unknown OID']
%

method:
at: aKey
ifAbsent: aBlock

	| element |
	element := self
		critical:
			[map
				at: aKey
				ifAbsent: [nil]].
	^element
		ifNil: aBlock
		ifNotNil: [element key]
%

method:
includes: aService

	^self critical: [map includesKey: aService rsrId]
%