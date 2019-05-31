"
I represent a class of Objects that know offer Rsr Services.
"
Class {
	#name : #RsrService,
	#superclass : #RsrObject,
	#instVars : [
		'remoteSelf'
	],
	#category : #'RemoteServiceReplication-Public'
}

{ #category : #accessing }
RsrService class >> clientClass [

	self subclassResponsibility
]

{ #category : #testing }
RsrService class >> isClientClass [

	^self == self clientClass
]

{ #category : #testing }
RsrService class >> isServerClass [

	^self == self serverClass
]

{ #category : #accessing }
RsrService class >> serverClass [

	self subclassResponsibility
]

{ #category : #'private-accessing' }
RsrService class >> variablesToReflect [

	| currentClass variables |
	variables := OrderedCollection new.
	currentClass := self superclass.
	[currentClass == RsrService]
		whileFalse:
			[currentClass instVarNames reverseDo: [:each | variables addFirst: each].
			currentClass := currentClass superclass].
	^variables
]

{ #category : #accessing }
RsrService >> isClient [

	^self class isClientClass
]

{ #category : #accessing }
RsrService >> isServer [

	^self class isServerClass
]

{ #category : #'private-lifecycle' }
RsrService >> markForFinalization [

	self isClient
		ifTrue:
			[self
				toFinalizeSend: #reap:
				to: self rsrConnection reaper
				with: self rsrId]
]

{ #category : #'private-lifecycle' }
RsrService >> registerWith: aRegistry [

	self isServer
		ifTrue: [aRegistry retain: self]
		ifFalse:
			[aRegistry register: self.
			self markForFinalization]
]

{ #category : #accessing }
RsrService >> remoteServiceClass [

	^self isClient
		ifTrue: [self class serverClass]
		ifFalse: [self class clientClass]
]

{ #category : #accessing }
RsrService >> remoteServiceName [

	^self remoteServiceClass name
]

{ #category : #'private-accessing' }
RsrService >> rsrConnection: aConnection [

	super rsrConnection: aConnection.
	remoteSelf := aConnection rsrForwarderClass on: self
]

{ #category : #'public-accessing' }
RsrService >> serviceName [

	^self class name
]

{ #category : #'public-synchronization' }
RsrService >> synchronize [

	remoteSelf yourself
]
