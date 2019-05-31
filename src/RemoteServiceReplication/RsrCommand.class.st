Class {
	#name : #RsrCommand,
	#superclass : #Object,
	#instVars : [
		'encoding'
	],
	#category : 'RemoteServiceReplication-Communications'
}

{ #category : #executing }
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

{ #category : #executing }
RsrCommand >> executeFor: aConnection [

	self subclassResponsibility
]

{ #category : #writing }
RsrCommand >> writeUsing: aCommandWriter [

	aCommandWriter write: encoding
]
