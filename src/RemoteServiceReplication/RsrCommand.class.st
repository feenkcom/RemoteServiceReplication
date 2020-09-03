Class {
	#name : 'RsrCommand',
	#superclass : 'RsrObject',
	#instVars : [
		'encoding'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'encoding' }
RsrCommand >> encode: aStream
using: anEncoder [

	self subclassResponsibility
]

{ #category : 'accessing' }
RsrCommand >> encoding [
	^ encoding
]

{ #category : 'accessing' }
RsrCommand >> encoding: anObject [
	encoding := anObject
]

{ #category : 'executing' }
RsrCommand >> executeFor: aConnection [

	self subclassResponsibility
]

{ #category : 'reporting' }
RsrCommand >> reportOn: aLog [

	self subclassResponsibility
]
