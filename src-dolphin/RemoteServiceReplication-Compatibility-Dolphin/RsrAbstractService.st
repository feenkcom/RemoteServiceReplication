RsrObject
	subclass: #RsrAbstractService
	instanceVariableNames: 'finalizationSend'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrClassResolver categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

!RsrAbstractService methodsFor!
toFinalizeEvaluate: aMessageSend

	finalizationSend := aMessageSend.
	self beFinalizable!

finalize

	finalizationSend value! !
