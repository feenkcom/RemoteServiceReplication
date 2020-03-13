Class {
	#name : #RsrForwarder,
	#superclass : #RsrProtoObject,
	#instVars : [
		'_object'
	],
	#category : #'RemoteServiceReplication-Compatibility-Pharo'
}

{ #category : #'private-forwarding' }
RsrForwarder >> _object: anRsrObject [

	_object := anRsrObject
]

{ #category : #'Glamour-Morphic-Brick' }
RsrForwarder >> asBrick [

	^_object asBrick
]

{ #category : #'GT-Spotter' }
RsrForwarder >> asSpotterCandidateLink [

	^_object asSpotterCandidateLink
]

{ #category : #'GT-Spotter' }
RsrForwarder >> asSpotterProcessorLink [

	^_object asSpotterProcessorLink
]

{ #category : #'Glamour-Morphic-Brick' }
RsrForwarder >> brickAlert [

	^_object brickAlert
]

{ #category : #'Glamour-Morphic-Brick' }
RsrForwarder >> brickValue: aBrick [

	^_object brickValue: aBrick
]

{ #category : #'Glamour-Morphic-Brick' }
RsrForwarder >> brickValue: anObject withEnoughArguments: aCollection [

	^_object brickValue: anObject withEnoughArguments: aCollection
]

{ #category : #'private-forwarding' }
RsrForwarder >> doesNotUnderstand: aMessage [

	| promise |
	promise := _object _connection
		_sendMessage: aMessage
		to: _object.
	^promise value
]

{ #category : #'GT-Debugger' }
RsrForwarder >> gtConstructDebuggerInspectorIn: composite
for: aDebugger [
	
	^_object
		gtConstructDebuggerInspectorIn: composite
		for: aDebugger
]

{ #category : #'GT-Debugger' }
RsrForwarder >> gtDebuggerEvaluatorIn: composite [
	
	^_object gtDebuggerEvaluatorIn: composite
]

{ #category : #'GT-Debugger' }
RsrForwarder >> gtDebuggerRawIn: composite [

	^_object gtDebuggerRawIn: composite
]

{ #category : #'GT-SUnitDebugger' }
RsrForwarder >> gtDebuggerSUnitPrint [

	^_object gtDebuggerSUnitPrint
]

{ #category : #'GT-Inspector' }
RsrForwarder >> gtDisplayOn: stream [

	^_object gtDisplayOn: stream
]

{ #category : #'GT-Inspector' }
RsrForwarder >> gtDisplayString [

	^_object gtDisplayString
]

{ #category : #'GT-Spotter' }
RsrForwarder >> gtFilter [

	^_object gtFilter
]

{ #category : #'GT-Spotter-UI' }
RsrForwarder >> gtInspectorActionSpot [

	^_object gtInspectorActionSpot
]

{ #category : #'GT-Inspector' }
RsrForwarder >> gtInspectorHash [

	^_object gtInspectorHash
]

{ #category : #'GT-Inspector' }
RsrForwarder >> gtInspectorIcon [

	^_object gtInspectorIcon
]

{ #category : #'GT-Inspector' }
RsrForwarder >> gtInspectorInterestingObject [

	^_object gtInspectorInterestingObject
]

{ #category : #'GT-Inspector' }
RsrForwarder >> gtInspectorRawIn: composite [

	^_object gtInspectorRawIn: composite
]

{ #category : #'GT-Inspector' }
RsrForwarder >> gtInspectorVariableNodesIn: aCollection [

	^_object gtInspectorVariableNodesIn: aCollection
]

{ #category : #'GT-Inspector' }
RsrForwarder >> gtInspectorVariableValuePairs [

	^_object gtInspectorVariableValuePairs
]

{ #category : #'GT-Spotter' }
RsrForwarder >> gtIterator [

	^_object gtIterator
]

{ #category : #'GT-Spotter' }
RsrForwarder >> gtListFilter [

	^_object gtListFilter
]

{ #category : #'GT-Spotter' }
RsrForwarder >> hasDynamicItems [

	^_object hasDynamicItems
]

{ #category : #'GT-Inspector' }
RsrForwarder >> inspectInExternalWindow [

	^_object inspectInExternalWindow
]

{ #category : #'Glamour-Morphic-Brick' }
RsrForwarder >> phlowValue: anObject [

	^_object phlowValue: anObject
]

{ #category : #'Glamour-Morphic-Brick' }
RsrForwarder >> phlowValue: anObject value: otherObject [

	^_object phlowValue: anObject value: otherObject
]

{ #category : #'Glamour-Morphic-Brick' }
RsrForwarder >> phlowValue: anObject withEnoughArguments: aCollection [

	^_object phlowValue: anObject withEnoughArguments: aCollection
]

{ #category : #printing }
RsrForwarder >> printStringLimitedTo: anInteger [

	^''
]

{ #category : #'Glamour-Helpers' }
RsrForwarder >> renderGlamorouslyOn: aRenderer [

	^_object renderGlamorouslyOn: aRenderer
]

{ #category : #'GT-Spotter' }
RsrForwarder >> spotterActDefault [

	^_object spotterActDefault
]

{ #category : #'GT-Spotter' }
RsrForwarder >> spotterActDefault: aStep [

	^_object spotterActDefault: aStep
]

{ #category : #'GT-Spotter' }
RsrForwarder >> spotterPreviewIn: aComposite inContext: aSpotter [

	^_object spotterPreviewIn: aComposite inContext: aSpotter
]

{ #category : #'GT-Spotter-UI' }
RsrForwarder >> spotterPreviewRawIn: aComposite [

	^_object spotterPreviewRawIn: aComposite
]

{ #category : #'GT-Spotter' }
RsrForwarder >> spotterProcessorsFor: aSpotterStep [

	^_object spotterProcessorsFor: aSpotterStep
]

{ #category : #'GT-Spotter' }
RsrForwarder >> spotterProcessorsFor: aSpotterStep pragma: aPragma [

	^_object spotterProcessorsFor: aSpotterStep pragma: aPragma
]

{ #category : #'GT-Spotter' }
RsrForwarder >> spotterRePropertiesFor: aStep [

	^_object spotterRePropertiesFor: aStep
]
