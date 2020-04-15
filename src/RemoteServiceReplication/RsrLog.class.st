Class {
	#name : 'RsrLog',
	#superclass : 'RsrObject',
	#instVars : [
		'verbosity'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'logging' }
RsrLog >> debug: aString [

	self verbosity >= self levelDebug
		ifTrue: [	self log: 'DEBUG: ', aString]
]

{ #category : 'logging' }
RsrLog >> error: aString [

	self verbosity >= self levelError
		ifTrue: [self log: 'ERROR: ', aString]
]

{ #category : 'logging' }
RsrLog >> info: aString [

	self verbosity >= self levelInfo
		ifTrue: [self log: 'INFO: ', aString]
]

{ #category : 'initialization' }
RsrLog >> initialize [

	super initialize.
	verbosity := self levelTrace
]

{ #category : 'accessing' }
RsrLog >> levelDebug [

	^4
]

{ #category : 'accessing' }
RsrLog >> levelError [

	^1
]

{ #category : 'accessing' }
RsrLog >> levelInfo [

	^3
]

{ #category : 'accessing' }
RsrLog >> levelTrace [

	^5
]

{ #category : 'accessing' }
RsrLog >> levelWarn [

	^2
]

{ #category : 'logging' }
RsrLog >> log: aString [

	Transcript
		show: RsrDateAndTimeSpecies now printString, '-', aString;
		cr
]

{ #category : 'logging' }
RsrLog >> log: aMessage
level: aLevelString [

	Transcript
		show: RsrDateAndTimeSpecies now printString, '-', aLevelString, '-', aMessage;
		cr
]

{ #category : 'logging' }
RsrLog >> trace: aString [

	self verbosity >= self levelTrace
		ifTrue: [self log: 'TRACE: ', aString]
]

{ #category : 'accessing' }
RsrLog >> verbosity [

	^verbosity
]

{ #category : 'accessing' }
RsrLog >> verbosity: aLogLevel [

	verbosity := aLogLevel
]

{ #category : 'logging' }
RsrLog >> warn: aString [

	self verbosity >= self levelDebug
		ifTrue: [self log: 'WARN: ', aString]
]
