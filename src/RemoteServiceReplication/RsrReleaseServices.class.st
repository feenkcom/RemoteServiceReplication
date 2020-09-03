"
No class-specific documentation for RsrReleaseServices, hierarchy is:
Object
  RsrObject
    RsrCommand( encoding)
      RsrReleaseServices( oids)

"
Class {
	#name : 'RsrReleaseServices',
	#superclass : 'RsrCommand',
	#instVars : [
		'sids'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'instance creation' }
RsrReleaseServices class >> sids: anArrayOfServiceIDs [

	^self new
		sids: anArrayOfServiceIDs;
		yourself
]

{ #category : 'encoding' }
RsrReleaseServices >> encode: aStream
using: anEncoder [

	anEncoder
		encodeReleaseServices: self
		onto: aStream
]

{ #category : 'executing' }
RsrReleaseServices >> executeFor: aConnection [

	| registry |
	registry := aConnection registry.
	sids do: [:sid | registry cleanupEntryFor: sid]
]

{ #category : 'reporting' }
RsrReleaseServices >> reportOn: aLog [

	aLog debug: 'RsrReleaseObjects(', self sids printString, ')'
]

{ #category : 'accessing' }
RsrReleaseServices >> sids [

	^sids
]

{ #category : 'accessing' }
RsrReleaseServices >> sids: anArrayOfServiceIDs [

	sids := anArrayOfServiceIDs
]
