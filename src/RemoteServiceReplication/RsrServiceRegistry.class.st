Class {
	#name : #RsrServiceRegistry,
	#superclass : #RsrAbstractServiceRegistry,
	#instVars : [
		'mutex',
		'services'
	],
	#category : 'RemoteServiceReplication-Public'
}

{ #category : #'public-accessing' }
RsrServiceRegistry >> at: aNameString [

	^self critical: [services at: aNameString]
]

{ #category : #'public-accessing' }
RsrServiceRegistry >> at: aNameString
ifAbsent: aBlock [

	^self
		critical:
			[services
				at: aNameString
				ifAbsent: aBlock]
]

{ #category : #'public-accessing' }
RsrServiceRegistry >> at: aNameString
put: aService [

	self
		register: aService
		for: aNameString.
	remoteSelf
		register: aService
		for: aNameString
		
]

{ #category : #'private-coordination' }
RsrServiceRegistry >> critical: aBlock [

	^mutex critical: aBlock
]

{ #category : #'private-registration' }
RsrServiceRegistry >> deregisterName: aServiceName [

	self critical: [services removeKey: aServiceName ifAbsent: []]
]

{ #category : #'private-initialization' }
RsrServiceRegistry >> initialize [

	super initialize.
	services := Dictionary new.
	mutex := Semaphore forMutualExclusion
]

{ #category : #'private-registration' }
RsrServiceRegistry >> register: aService
for: aNameString [

	self
		critical:
			[services
				at: aNameString
				put: aService]
]

{ #category : #'public-accessing' }
RsrServiceRegistry >> removeKey: aServiceName [

	self deregisterName: aServiceName.
	remoteSelf deregisterName: aServiceName
]
