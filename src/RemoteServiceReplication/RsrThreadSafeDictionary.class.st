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

{ #category : 'new interface' }
RsrThreadSafeDictionary >> at: aKey
ifAbsent: aBlock [

	^mutex critical: [map at: aKey ifAbsent: aBlock]
]

{ #category : 'new interface' }
RsrThreadSafeDictionary >> at: aKey
put: aValue [

	mutex critical: [map at: aKey put: aValue].
	^aValue
]

{ #category : 'initialization' }
RsrThreadSafeDictionary >> initialize [

	super initialize.
	mutex := Semaphore forMutualExclusion.
	map := Dictionary new
]

{ #category : 'new interface' }
RsrThreadSafeDictionary >> removeKey: anRsrId [

	^mutex critical: [map removeKey: anRsrId ifAbsent: [nil]]
]
