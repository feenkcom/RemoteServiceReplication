"
No class-specific documentation for RsrInstrumentedClient, hierarchy is:
Object
  RsrObject
    RsrAbstractService
      RsrService( _id _connection remoteSelf)
        RsrInstrumentedService
          RsrInstrumentedClient( preUpdateAction postUpdateAction)

"
Class {
	#name : 'RsrInstrumentedClient',
	#superclass : 'RsrInstrumentedService',
	#instVars : [
		'preUpdateCount',
		'postUpdateCount'
	],
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'accessing' }
RsrInstrumentedClient >> postUpdateCount [

	^postUpdateCount ifNil: [0]
]

{ #category : 'accessing' }
RsrInstrumentedClient >> postUpdateCount: anInteger [

	postUpdateCount := anInteger
]

{ #category : 'accessing' }
RsrInstrumentedClient >> preUpdateCount [

	^preUpdateCount ifNil: [0]
]

{ #category : 'accessing' }
RsrInstrumentedClient >> preUpdateCount: anInteger [

	preUpdateCount := anInteger
]

{ #category : 'evaluating' }
RsrInstrumentedClient >> return: anObject [

	^remoteSelf return: anObject
]

{ #category : 'evaluating' }
RsrInstrumentedClient >> value [

	^remoteSelf value
]
