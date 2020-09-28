"
No class-specific documentation for RsrInstrumentedServer, hierarchy is:
Object
  RsrObject
    RsrAbstractService
      RsrService( _id _connection remoteSelf)
        RsrInstrumentedService
          RsrInstrumentedServer( preUpdateAction postUpdateAction)

"
Class {
	#name : 'RsrInstrumentedServer',
	#superclass : 'RsrInstrumentedService',
	#instVars : [
		'preUpdateCount',
		'postUpdateCount',
		'action'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrInstrumentedServer >> action [

	^action
]

{ #category : 'accessing' }
RsrInstrumentedServer >> action: aBlock [

	action := aBlock
]

{ #category : 'accessing' }
RsrInstrumentedServer >> postUpdateCount [

	^postUpdateCount ifNil: [0]
]

{ #category : 'accessing' }
RsrInstrumentedServer >> postUpdateCount: anInteger [

	postUpdateCount := anInteger
]

{ #category : 'accessing' }
RsrInstrumentedServer >> preUpdateCount [

	^preUpdateCount ifNil: [0]
]

{ #category : 'accessing' }
RsrInstrumentedServer >> preUpdateCount: anInteger [

	preUpdateCount := anInteger
]

{ #category : 'evaluating' }
RsrInstrumentedServer >> return: anObject [

	^anObject
]

{ #category : 'evaluating' }
RsrInstrumentedServer >> value [

	^self action value
]
