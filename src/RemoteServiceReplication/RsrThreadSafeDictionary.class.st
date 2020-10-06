"
I maintain the associations between locally stored objects and their remote counterparts.
"
Class {
	#name : 'RsrThreadSafeDictionary',
	#superclass : 'RsrObject',
	#instVars : [
		'mutex',
		'map'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'accessing' }
RsrThreadSafeDictionary >> at: aKey
ifAbsent: aBlock [

	| isPresent result |
	isPresent := true.
	result := mutex critical: [map at: aKey ifAbsent: [isPresent := false]].
	^isPresent
		ifTrue: [result]
		ifFalse: [aBlock value]
]

{ #category : 'accessing' }
RsrThreadSafeDictionary >> at: aKey
put: aValue [

	mutex critical: [map at: aKey put: aValue].
	^aValue
]

{ #category : 'enumerating' }
RsrThreadSafeDictionary >> do: aBlock [

	| values |
	values := mutex critical: [map values].
	values do: aBlock
]

{ #category : 'initialization' }
RsrThreadSafeDictionary >> initialize [

	super initialize.
	mutex := Semaphore forMutualExclusion.
	map := Dictionary new
]

{ #category : 'removing' }
RsrThreadSafeDictionary >> removeKey: anRsrId [

	^mutex critical: [map removeKey: anRsrId ifAbsent: [nil]]
]

{ #category : 'removing' }
RsrThreadSafeDictionary >> removeKey: anRsrId
ifAbsent: aBlock [

	| element wasRemoved |
	wasRemoved := true.
	element := mutex critical: [map removeKey: anRsrId ifAbsent: [wasRemoved := false]].
	^wasRemoved
		ifTrue: [element]
		ifFalse: [aBlock value]
]
