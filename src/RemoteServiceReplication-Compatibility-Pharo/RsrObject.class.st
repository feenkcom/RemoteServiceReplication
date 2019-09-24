Class {
	#name : #RsrObject,
	#superclass : #Object,
	#classInstVars : [
		'nativeClasses'
	],
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #accessing }
RsrObject class >> nativeClasses [
	"Return the collection of classes supported by RSR in this environment"

	^Set withAll: RsrSpecies speciesMapping keys
]

{ #category : #'instance creation' }
RsrObject class >> new [

	^super new
]

{ #category : #initialization }
RsrObject >> initialize [

	^self
]

{ #category : #accessing }
RsrObject >> nativeClasses [
	"Return the collection of classes supported by RSR in this environment"

	^self class nativeClasses
]
