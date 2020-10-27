Class {
	#name : 'RsrPromise',
	#superclass : 'RsrObject',
	#instVars : [
		'mutex',
		'value',
		'state',
		'resolvedMutex',
		'resolutionActions'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'private-asserting' }
RsrPromise >> assertNotResolved [

	self isResolved
		ifTrue: [RsrPromiseAlreadyResolved signal].
]

{ #category : 'fulfillment' }
RsrPromise >> break: aReason [

	mutex
		critical:
			[self assertNotResolved.
			value := aReason.
			state := #Broken].
	self notifyActions.
	resolvedMutex signal
]

{ #category : 'fulfillment' }
RsrPromise >> error: anException [
	"Temporary synonym of #break:"

	^self break: anException
]

{ #category : 'fulfillment' }
RsrPromise >> fulfill: anObject [

	mutex
		critical:
			[self assertNotResolved.
			value := anObject.
			state := #Fulfilled].
	self notifyActions.
	resolvedMutex signal
]

{ #category : 'initialization' }
RsrPromise >> initialize [

	super initialize.
	mutex := Semaphore forMutualExclusion.
	resolvedMutex := Semaphore new.
	state := #PendingResolution.
	resolutionActions := OrderedCollection new
]

{ #category : 'testing' }
RsrPromise >> isBroken [

	^state == #Broken
]

{ #category : 'testing' }
RsrPromise >> isFulfilled [

	^state == #Fulfilled
]

{ #category : 'testing' }
RsrPromise >> isResolved [

	^self isBroken or: [self isFulfilled]
]

{ #category : 'fulfillment' }
RsrPromise >> notifyActions [

	| actions |
	mutex
		critical:
			[actions := resolutionActions.
			resolutionActions := OrderedCollection new].
	actions
		do:
			[:each |
			self isFulfilled
				ifTrue: [RsrProcessModel fork: [each when value: value]]
				ifFalse: [RsrProcessModel fork: [each catch value: value]]]
]

{ #category : 'synchronous-accessing' }
RsrPromise >> value [
	"Alias for #wait"

	^self wait
]

{ #category : 'synchronous-accessing' }
RsrPromise >> wait [
	"Wait for a the receiver to be Resolved.
	If fulfilled - return the fulfillment value.
	If broken - raise an RsrBrokenPromise exception w/ the reason."

	self waitForResolution.
	^self isBroken
		ifTrue: [RsrBrokenPromise signalReason: value]
		ifFalse: [value]
]

{ #category : 'private-waiting' }
RsrPromise >> waitForResolution [
	"There doesn't seem to be a great way to implement this method.
	The ensure below is generally safe but does have a side-effect of signaling
	the mutex when the process is terminated while waiting.
	Removing the ensure allows the signal to be lost if the process is terminated
	just after #wait but before #signal is processed.
	In order to solve this, the loop verifies the promise is actually resolved before
	continuing."

	self isResolved
		ifTrue: [^self].
	[[self isResolved] whileFalse: [resolvedMutex wait]] ensure: [resolvedMutex signal]
]

{ #category : 'other' }
RsrPromise >> when: aWhenBlock
catch: aCatchBlock [

	| action shouldNotifyActions |
	action := RsrPromiseResolutionAction
		when: aWhenBlock
		catch: aCatchBlock.
	mutex
		critical:
			[shouldNotifyActions := self isResolved.
			resolutionActions add: action].
	shouldNotifyActions ifTrue: [self notifyActions]
]
