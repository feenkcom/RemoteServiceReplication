"
I represent an object that's being managed by Rsr.
"
Class {
	#name : #RsrObject,
	#superclass : #Object,
	#instVars : [
		'rsrId',
		'rsrConnection'
	],
	#category : #'RemoteServiceReplication-Base'
}

{ #category : #'instance creation' }
RsrObject class >> rsrId: anId
rsrConnection: aConnection [

	^super new
		rsrId: anId;
		rsrConnection: aConnection;
		yourself
]

{ #category : #accessing }
RsrObject class >> variablesToReflect [

	^self allInstVarNames
]

{ #category : #'instance creation' }
RsrObject class >> wrap: anObject [
	|rsr|
	rsr := RsrObject new.
	rsr put: anObject.
	^rsr
]

{ #category : #'public-testing' }
RsrObject >> isMirrored [

	^rsrConnection ~~ nil
]

{ #category : #'public-testing' }
RsrObject >> isNotMirrored [

	^self isMirrored not
]

{ #category : #'private-lifecycle' }
RsrObject >> registerWith: aRegistry [

	aRegistry register: self
]

{ #category : #'private-accessing' }
RsrObject >> rsrConnection [

	^rsrConnection
]

{ #category : #'private-accessing' }
RsrObject >> rsrConnection: aConnection [

	rsrConnection := aConnection
]

{ #category : #'private-accessing' }
RsrObject >> rsrId [

	^rsrId
]

{ #category : #'private-accessing' }
RsrObject >> rsrId: anRsrId [

	rsrId := anRsrId
]
