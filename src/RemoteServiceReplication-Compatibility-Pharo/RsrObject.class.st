Class {
	#name : #RsrObject,
	#superclass : #Object,
	#classInstVars : [
		'nativeClasses'
	],
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #'instance creation' }
RsrObject class >> new [

	^super new
]

{ #category : #initialization }
RsrObject >> initialize [

	^self
]

{ #category : #accessing }
RsrObject >> instruments [

	^RsrInstruments new
]
