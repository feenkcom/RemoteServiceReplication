Class {
	#name : #RsrSpeciesReturnEquality,
	#superclass : #RsrSystemTestCase,
	#category : #'RemoteServiceReplication-Test-TestCases'
}

{ #category : #accessing }
RsrSpeciesReturnEquality >> basicExamples [
	"Give a samples of each species to ensure Collection classes are able to encode each type successfully."

	^{RsrClientNoInstVars new.
	#h.
	'h'.
	0.
	234.
	-97.
	$s.
	nil.
	true.
	false.
	{RsrClientNoInstVars new. {}.}.
	#[123].
	Set with: 42.
	OrderedCollection with: #x.
	Dictionary new at: #key put: #value; yourself.
	RsrDateAndTimeSpecies fromMicroseconds: -1000000. "1969-12-31T23:59:59-00:00"}
]

{ #category : #running }
RsrSpeciesReturnEquality >> testArray [

	self
		verify: #();
		verify: (Array withAll: self basicExamples)
]

{ #category : #running }
RsrSpeciesReturnEquality >> testBoolean [

	self
		verify: true;
		verify: false
]

{ #category : #running }
RsrSpeciesReturnEquality >> testByteArray [

	self
		verify: #[];
		verify: (ByteArray withAll: (0 to: 255));
		verify: (ByteArray new: 1024)
]

{ #category : #running }
RsrSpeciesReturnEquality >> testDateAndTime [

	self
		verify: RsrDateAndTimeSpecies now;
		verify: (RsrDateAndTimeSpecies fromMicroseconds: -491277642567488); "1954-06-07T14:59:17.432512-07:00"
		verify: (RsrDateAndTimeSpecies fromMicroseconds: 1562692562657612). "2019-07-09T10:16:02.657612-07:00"
]

{ #category : #running }
RsrSpeciesReturnEquality >> testDictionary [

	| example |
	example := Dictionary new.
	self verify: example.
	self basicExamples do: [:each | each ifNotNil: [example at: each put: each]].
	example at: 1 put: nil.
	self verify: example
]

{ #category : #running }
RsrSpeciesReturnEquality >> testInteger [

	self
		verify: 0;
		verify: -1;
		verify: 1;
		verify: (2 raisedTo: 32);
		verify: (2 raisedTo: 32) negated;
		verify: (2 raisedTo: 64);
		verify: (2 raisedTo: 64) negated;
		verify: 4598754392654025898794;
		verify: -13750198234577893465
]

{ #category : #running }
RsrSpeciesReturnEquality >> testOrderedCollection [

	self
		verify: OrderedCollection new;
		verify: (OrderedCollection withAll: self basicExamples)
]

{ #category : #running }
RsrSpeciesReturnEquality >> testSet [

	self
		verify: Set new;
		verify: (Set withAll: self basicExamples)
]

{ #category : #running }
RsrSpeciesReturnEquality >> testString [

	self
		verify: '';
		verify: 'string'
]

{ #category : #running }
RsrSpeciesReturnEquality >> testSymbol [

	self
		verify: #'';
		verify: #symbol
]

{ #category : #running }
RsrSpeciesReturnEquality >> testUndefinedObject [

	self verify: nil
]

{ #category : #asserting }
RsrSpeciesReturnEquality >> verify: anObject [
	"Send <anObject> through RSR and have it returned. Assert it is equivalent."

	| result |
	result := (connectionA serviceFor: #RsrClientNoInstVars) sendReturnArgument: anObject.
	self
		assert: result
		equals: anObject
]
