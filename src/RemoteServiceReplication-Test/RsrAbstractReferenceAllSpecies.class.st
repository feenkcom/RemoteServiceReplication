Class {
	#name : 'RsrAbstractReferenceAllSpecies',
	#superclass : 'RsrService',
	#instVars : [
		'emptyService',
		'serviceReferenceService',
		'emptySymbol',
		'symbol',
		'emptyString',
		'string',
		'largeNegative',
		'smallNegative',
		'zero',
		'smallPositive',
		'largePositive',
		'asciiCharacter',
		'undefinedObject',
		'booleanTrue',
		'booleanFalse',
		'emptyArray',
		'array',
		'emptyByteArray',
		'byteArray',
		'emptyOrderedCollection',
		'orderedCollection',
		'emptyDictionary',
		'dictionary',
		'epoch',
		'beforeEpoch',
		'afterEpoch',
		'arrayWithAllTypes',
		'emptySet',
		'set',
		'setWithAllTypes',
		'orderedCollectionWithAllTypes',
		'dictionaryWithAllTypes',
		'unicodeCharacter'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrAbstractReferenceAllSpecies class >> abstractClassName [

	^#RsrAbstractReferenceAllSpecies
]

{ #category : 'accessing' }
RsrAbstractReferenceAllSpecies class >> clientClassName [

	^#RsrReferenceAllSpeciesClient
]

{ #category : 'accessing' }
RsrAbstractReferenceAllSpecies class >> serverClassName [

	^#RsrReferenceAllSpeciesServer
]

{ #category : 'initialization' }
RsrAbstractReferenceAllSpecies >> initialize [

	super initialize.
	emptyService := RsrClientNoInstVars new.
	serviceReferenceService := RsrClientReferenceService service: RsrClientNoInstVars new.
	emptySymbol := #''.
	symbol := #Symbol.
	emptyString := ''.
	string := 'String'.
	largeNegative := -879021356918273469872356.
	smallNegative := -1.
	zero := 0.
	smallPositive := 1.
	largePositive := 29183756217893561289745689732456.
	asciiCharacter := $e.
	unicodeCharacter := Character codePoint: 16r01D4.
	undefinedObject := nil.
	booleanTrue := true.
	booleanFalse := false.
	emptyArray := #().
	array := {emptyService. serviceReferenceService. emptySymbol. symbol. emptyString. string. largeNegative. smallNegative. zero. smallPositive. largePositive. asciiCharacter. unicodeCharacter. undefinedObject. booleanTrue. booleanFalse. emptyArray.}.
	emptyByteArray := #[].
	byteArray := #[123 45 67].
	emptySet := Set new.
	set := Set withAll: array.
	emptyOrderedCollection := OrderedCollection new.
	orderedCollection := OrderedCollection withAll: array.
	emptyDictionary := Dictionary new.
	dictionary := Dictionary new.
	array doWithIndex: [:each :i | dictionary at: i put: each].
	beforeEpoch := (RsrDateAndTimeSpecies fromMicroseconds: -491277642567488). "1954-06-07T14:59:17.432512-07:00"
	epoch := RsrDateAndTimeSpecies posixEpoch.
	afterEpoch := (RsrDateAndTimeSpecies fromMicroseconds: 1562692562657612). "2019-07-09T10:16:02.657612-07:00"
	arrayWithAllTypes := array, {emptyArray. array. emptyByteArray. byteArray. emptySet. set. emptyOrderedCollection. orderedCollection. emptyDictionary. dictionary. beforeEpoch. epoch. afterEpoch.}.
	setWithAllTypes := Set withAll: arrayWithAllTypes.
	orderedCollectionWithAllTypes := OrderedCollection withAll: arrayWithAllTypes.
	dictionaryWithAllTypes := Dictionary new.
	arrayWithAllTypes doWithIndex: [:each :i | dictionaryWithAllTypes at: i put: each]
	
]
