# Concurrent Dispatching to Services

The purpose of this document is to describe the concurrent dispatch model used in RSR.

## Test Case Examples - Ideal 1

In this situation, a service is aware of the dispatcher from which is receives messages. A shared dispatcher should result in sharing in both peers. 

```smalltalk
testSharedDispatcher
	"Ensure that services sharing a dispatcher are have events which are evaluated in serial (aka. they use the same process.)"

	| dispatcher server1 server2 client1 client2 |
	dispatcher := RsrTestMessageDispatcher startOn: connectionA.
	server1 :=  RsrConcurrentTestServer dispatcher: dispatcher.
	server2 :=  RsrConcurrentTestServer dispatcher: dispatcher.
	client1 := self mirror: server1.
	client2 := self mirror: server2.
	client1 stashProcess.
	client2 stashProcess.
	self
		assert: client1 dispatcher
		identicalTo: client2 dispatcher.
	self
		assert: server1 dispatcher
		identicalTo: server2 dispatcher
```

```smalltalk
testCreateServiceWithDispatcher
	"When a service is created, we should be able to specify a specific dispatcher.
	Messages that need to be sent to the service will need to be dispatched through."

	| dispatcher server client |
	dispatcher := RsrTestMessageDispatcher startOn: connectionA.
	server :=  RsrConcurrentTestServer dispatcher: dispatcher.
	client := self mirror: server.
	client stashProcess.
	self
		assert: server stashedProcess
		identicalTo: dispatcher process
```
