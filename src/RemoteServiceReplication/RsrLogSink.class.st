Class {
	#name : #RsrLogSink,
	#superclass : #RsrObject,
	#category : 'RemoteServiceReplication'
}

{ #category : #writing }
RsrLogSink >> write: aMessage [

	self subclassResponsibility
]
