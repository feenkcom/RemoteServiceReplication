Class {
	#name : 'RsrMessageSendingTest',
	#superclass : 'RsrSystemTestCase',
	#category : 'RemoteServiceReplication-Test'
}

{ #category : 'testing' }
RsrMessageSendingTest class >> isAbstract [

	^self == RsrMessageSendingTest
]

{ #category : 'expecting' }
RsrMessageSendingTest >> expectCatch: aPromise [

	| semaphore wasFulfilled result |
	semaphore := Semaphore new.
	wasFulfilled := false.
	aPromise
		when: [:value | wasFulfilled := true. semaphore signal]
		catch: [:reason | result := reason. semaphore signal].
	semaphore wait.
	self deny: wasFulfilled.
	^result
]

{ #category : 'expecting' }
RsrMessageSendingTest >> expectWhen: aPromise [

	| semaphore wasBroken result |
	semaphore := Semaphore new.
	wasBroken := false.
	aPromise
		when: [:value | result := value. semaphore signal]
		catch: [:r | wasBroken := true. semaphore signal].
	semaphore wait.
	self deny: wasBroken.
	^result
]

{ #category : 'running-errors' }
RsrMessageSendingTest >> testAsyncRemoteError [

	| client server promise semaphore whenRan reason |
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [Error new tag: 'tag'; messageText: 'messageText'; signal].
	promise := client asyncValue.
	semaphore := Semaphore new.
	whenRan := false.
	promise
		when: [:x | whenRan := true. semaphore signal]
		catch: [:theReason | reason := theReason. semaphore signal].
	semaphore wait.
	self deny: whenRan.
	self assert: reason isRemoteException.
	self
		assert: reason exceptionClassName
		equals: #Error.
	self
		assert: reason tag
		equals: 'tag'.
	self
		assert: reason messageText
		equals: 'messageText'.
	self
		assert: reason stack isString;
		assert: reason stack size > 0
]

{ #category : 'running' }
RsrMessageSendingTest >> testAsyncReturnArgument [

	| client server promise returnedArgument semaphore catchRan |
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [:arg | arg].
	promise := client asyncValue: client.
	semaphore := Semaphore new.
	catchRan := false.
	promise
		when: [:service | returnedArgument := service. semaphore signal]
		catch: [:reason | catchRan := true. semaphore signal].
	semaphore wait.
	self deny: catchRan.
	self
		assert: returnedArgument
		identicalTo: client
]

{ #category : 'running' }
RsrMessageSendingTest >> testAsyncReturnService [

	| client server promise returnedService semaphore catchRan |
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [RsrValueHolderServer new].
	promise := client asyncValue.
	semaphore := Semaphore new.
	catchRan := false.
	promise
		when: [:service | returnedService := service. semaphore signal]
		catch: [:reason | catchRan := true. semaphore signal].
	semaphore wait.
	self deny: catchRan.
	self
		assert: returnedService class
		equals: RsrValueHolderClient
]

{ #category : 'running' }
RsrMessageSendingTest >> testChangeRemoteState [

	| marker client server |
	marker := false.
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [marker := true].
	client value.
	self assert: marker
]

{ #category : 'running-errors' }
RsrMessageSendingTest >> testDebugHandlerBreak [
	"Ensure that if a debug handler resolves the message,
	that the correct reason is received remotely."

	| marker client server reason |
	marker := #testMarker.
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [RsrResumableError signal. 42 "ensure we do not return the marker"].
	server debugHandler: [:exception :messageSend :resolver | resolver break: marker. nil "ensure we do not return the marker"].
	reason := self expectCatch: client asyncValue.
	self
		assert: reason
		equals: marker.
	server action: [RsrNonresumableError signal. 42 "ensure we do not return the marker"].
	reason := self expectCatch: client asyncValue.
	self
		assert: reason
		equals: marker
]

{ #category : 'running-errors' }
RsrMessageSendingTest >> testDebugHandlerException [
	"Ensure that an exception that occurs in the debug handler
	is reported back as the reason for Promise breaking."

	| marker client server reason |
	marker := #testMarker.
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [RsrResumableError signal].
	server debugHandler: [:exception :messageSend :resolver | Error signal].
	reason := self expectCatch: client asyncValue.
	self
		assert: reason exceptionClassName
		equals: #Error.
	server action: [RsrNonresumableError signal].
	reason := self expectCatch: client asyncValue.
	self
		assert: reason exceptionClassName
		equals: #Error.
]

{ #category : 'running-errors' }
RsrMessageSendingTest >> testDebugHandlerFulfill [
	"Ensure that if a debug handler resolves the message,
	that the fulfillment value is received remotely."

	| marker client server |
	marker := #testMarker.
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [RsrResumableError signal. 42 "ensure we do not return the marker"].
	server debugHandler: [:exception :messageSend :resolver | resolver fulfill: marker. nil "ensure we do not return the marker"].
	self
		assert: client value
		equals: marker.
	server action: [RsrNonresumableError signal. 42 "ensure we do not return the marker"].
	self
		assert: client value
		equals: marker
]

{ #category : 'running-errors' }
RsrMessageSendingTest >> testDebugHandlerNoResolutionWithNonresumableException [
	"Ensure that if the debug handler does not resolve the exception
	and the exception is nonresumable, that we Break the Promise
	reporting on the unresolved exception."

	| marker client server reason |
	marker := #testMarker.
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [RsrNonresumableError signal].
	server debugHandler: [:exception :messageSend :resolver | marker].
	reason := self expectCatch: client asyncValue.
	self
		assert: reason exceptionClassName
		equals: #RsrNonresumableError
]

{ #category : 'running-errors' }
RsrMessageSendingTest >> testDebugHandlerNoResolutionWithResumableException [
	"Ensure that if the debug handler does not resolve the exception
	and the exception is resumable, we resume with the evaluation
	result of the debug handler."

	| marker client server |
	marker := #testMarker.
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [RsrResumableError signal].
	server debugHandler: [:exception :messageSend :resolver | marker].
	self
		assert: client value
		equals: marker
]

{ #category : 'running-errors' }
RsrMessageSendingTest >> testRemoteError [

	| client server reason |
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [Error new tag: 'tag'; messageText: 'messageText'; signal].
	[client value]
		on: RsrBrokenPromise
		do: [:ex | reason := ex reason. ex return].
	self assert: reason isRemoteException.
	self
		assert: reason exceptionClassName
		equals: #Error.
	self
		assert: reason tag
		equals: 'tag'.
	self
		assert: reason messageText
		equals: 'messageText'.
	self
		assert: reason stack isString;
		assert: reason stack size > 0
]

{ #category : 'running-errors' }
RsrMessageSendingTest >> testRemoteErrorWithTag [

	| client server tag messageText reason |
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	tag := nil.
	messageText := 'messageText'.
	server action: [Error new tag: tag; messageText: messageText; signal].
	reason := [client value]
		on: RsrBrokenPromise
		do: [:ex | ex return: ex reason].
	self assert: reason isRemoteException.
	self
		assert: reason tag
		equals: 'messageText'.
	self
		assert: reason messageText
		equals: 'messageText'.
	self
		assert: reason stack isString;
		assert: reason stack size > 0.
	tag := 42.
	reason := [client value]
		on: RsrBrokenPromise
		do: [:ex | ex return: ex reason].
	self
		assert: reason tag
		equals: '42'.
	tag := RsrSignalErrorInAsString new.
	reason := [client value]
		on: RsrBrokenPromise
		do: [:ex | ex return: ex reason].
	self
		assert: reason tag
		equals: 'Unable to pack #tag containing an instance of RsrSignalErrorInAsString'
]

{ #category : 'running' }
RsrMessageSendingTest >> testReturnAlsoUpdatesLocalService [
	"Ensure that when the remote peer service returns a value,
	that it is also sent to update the local service."

	| client server value response |
	client := self serviceFactoryA serviceFor: #RsrReflectedVariableTestServiceB.
	client synchronize.
	server := connectionB serviceAt: client _id.
	value := 42.
	self
		deny: client varA
		equals: value.
	self
		deny: client varB
		equals: value.
	response := client setVarsToAndReturn: value.
	self
		assert: response
		equals: value.
	self
		assert: server varA
		equals: value.
	self
		assert: server varB
		equals: value.
	self
		assert: client varA
		equals: value.
	self
		assert: client varB
		equals: value
]

{ #category : 'running' }
RsrMessageSendingTest >> testReturnArgument [

	| client server arguments dt response |
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [:object | object].
	arguments := OrderedCollection new
		addAll: #( '' #symbol 'string' $h 0 -14 14 18446744073709551616 -18446744073709551616 nil true false );
 		add: (Character codePoint: 16r259F);
		add: (Dictionary new at: 1 put: 2; yourself);
		add: (Set with: 14);
		add: #[1 2 3 4];
		add: (OrderedCollection with: 42 with: 43);
		add: #(1 2 #(nil));
		yourself.
	dt := RsrDateAndTime now.
	response := client value: dt.
	self
		assert: (dt asSeconds * 1000000) rounded
		equals: (response asSeconds * 1000000) rounded.
	arguments
		do:
			[:each | | result |
			result := client value: each.
			self
				assert: result
				equals: each].
	arguments
		do:
			[:each | | result |
			result := server value: each.
			self
				assert: result
				equals: each].
	self
		assert: (client value: arguments)
		equals: arguments.
	self
		assert: (server value: arguments)
		equals: arguments.
	self
		assert: (client value: client)
		identicalTo: client
]

{ #category : 'running-errors' }
RsrMessageSendingTest >> testReturnInvalidObject [

	| client server reason |			
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [Object new].
	self
		should: [client value]
		raise: RsrBrokenPromise.
	reason := [client value]
		on: RsrBrokenPromise
		do: [:ex | ex return: ex reason].
	self assert: reason isRemoteException.
	self
		assert: reason exceptionClassName
		equals: #RsrUnsupportedObject.
	self
		assert: reason tag
		equals: 'Instances of Object cannot be serialized'.
	self
		assert: reason messageText
		equals: 'Instances of Object cannot be serialized'.
	self
		assert: reason stack isString;
		assert: reason stack size > 0
]

{ #category : 'running' }
RsrMessageSendingTest >> testReturnNewService [

	| client server returnedService |
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [RsrValueHolderServer new].
	returnedService := client value.
	self
		assert: returnedService class
		equals: RsrValueHolderClient
]

{ #category : 'running' }
RsrMessageSendingTest >> testReturnNewServiceInArray [

	| client server array returnedService |
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [Array with: RsrValueHolderServer new].
	array := client value.
	returnedService := array first.
	self
		assert: returnedService class
		equals: RsrValueHolderClient
]

{ #category : 'running' }
RsrMessageSendingTest >> testReturnSymbol [

	| client server symbol result |
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	symbol := #testSymbol.
	server action: [symbol].
	result := client value.
	self
		assert: result
		equals: symbol
]

{ #category : 'running-errors' }
RsrMessageSendingTest >> testSendInvalidObject [

	| client server |			
	client := connectionA serviceFor: #RsrRemoteAction.
	client synchronize.
	server := connectionB serviceAt: client _id.
	server action: [:arg | arg].
	self
		should: [client value: Object new]
		raise: RsrUnsupportedObject
]
