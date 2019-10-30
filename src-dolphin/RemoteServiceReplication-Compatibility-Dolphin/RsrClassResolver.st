RsrObject
	subclass: #RsrClassResolver
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
!RsrClassResolver categoriesForClass!RemoteServiceReplication-Compatibility-Dolphin! !

!RsrClassResolver class methodsFor!
classNamed: aSymbol

	^self
		classNamed: aSymbol
		ifAbsent: [RsrUnknownClass signal: aSymbol]!

classNamed: aSymbol
ifAbsent: aBlock

	^Smalltalk
		at: aSymbol
		ifAbsent: aBlock! !
