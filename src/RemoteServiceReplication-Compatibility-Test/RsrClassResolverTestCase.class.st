Class {
	#name : #RsrClassResolverTestCase,
	#superclass : #RsrTestCase,
	#category : #'RemoteServiceReplication-Compatibility-Test'
}

{ #category : #asserting }
RsrClassResolverTestCase >> assert: aClassName
resolvesTo: expectedClass [

	| actualClass |
	actualClass := RsrClassResolver classNamed: aClassName.
	self
		assert: actualClass
		identicalTo: expectedClass
]

{ #category : #running }
RsrClassResolverTestCase >> testFailedResolution [

	| actual marker |
	self
		should: [RsrClassResolver classNamed: #Xlerb]
		raise: Error.
	marker := Object new.
	actual := RsrClassResolver
		classNamed: #Xlerb
		ifAbsent: [marker].
	self
		assert: actual
		identicalTo: marker
]

{ #category : #running }
RsrClassResolverTestCase >> testSuccessfulResolution [

	| actual |
	actual := RsrClassResolver classNamed: #Object.
	self
		assert: actual
		identicalTo: Object.
	actual := RsrClassResolver
		classNamed: #Object
		ifAbsent: [self assert: false].
	self
		assert: actual
		identicalTo: Object
]
