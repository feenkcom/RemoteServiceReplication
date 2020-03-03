Class {
	#name : #RsrCommand,
	#superclass : #RsrObject,
	#instVars : [
		'encoding'
	],
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #lifecycle }
RsrCommand >> encodeUsing: anRsrEncoder [

	self subclassResponsibility
]

{ #category : #accessing }
RsrCommand >> encoding [
	^ encoding
]

{ #category : #accessing }
RsrCommand >> encoding: anObject [
	encoding := anObject
]

{ #category : #lifecycle }
RsrCommand >> executeFor: aConnection [

	self subclassResponsibility
]

{ #category : #reporting }
RsrCommand >> reportOn: aLog [

	self subclassResponsibility
]

{ #category : #lifecycle }
RsrCommand >> sendOver: aConnection [
	"Do nothing unless a subclass offers a specialization"
]

{ #category : #lifecycle }
RsrCommand >> writeUsing: aCommandWriter [

	aCommandWriter write: encoding
]
