# Remote Service Replication (Draft 3)

To provide a means of bridging two disparate Smalltalk environments allowing the sharing of resources.

## High-level Operations

* Send message to service partner

## Requirements of Smalltalk Environment

* Object Finalization
* Weak value collections
* Concurrency model

## Types of Objects

* Data Objects
  * Integer
  * String
  * TimeStamp
  * Character
  * Set
  * Dictionary
  * Array
  * ByteArray
  * OrderedCollection

## Features of RSR

* Transfer objects between environments
* Provide service abstraction
* Provide notion of identity to service across environments
* Expose network instead of trying to abstract it away -- What should this mean?

## Low-level Message Types:

* #RetainService
* #SendMessage
* #DeliverResponse
* #ReleaseServices

### RetainService

This message instructs the remote environment to strongly retain the embedded object until a message send or DeliverResponse is processed. Each service in the transitive closure of the message send will be encapsulated in an independence RetainService message.

### SendMessage

Instruct the framework to send a message. The receiver, selector, and arguments are included in the message. The result is returned via a DeliverResponse message. Should an error occur, this should be return via a DeliverResponse containing an exception.

### DeliverResponse

Contains a transaction ID and a reference to the object that should serve as a response.

### ReleaseServices

Contains (a) reference(s) to services which are no longer mirrored remotely.

## Service Lifetimes

Services come in two variations: Client and Server. The lifetime of services is defined by their Client instance. A Server will exist for at least as long as its Client exists.

All services received from another environment must be strongly retained until consumed by an incoming message or return.

## Service Creation

The initial service will be registered by the framework. It's purpose will be to facilitate the creation of additional services.

## Object Serialization

The framework will serialize and transfer objects in two separate ways.

* Mirroring
* Copying

### Mirroring

Classes that inherit from RsrService will mirror when sent to a bridged environment. Changes to a mirrored service will result in the change propagating to the bridged environment during a coordination window.

### Copying

Other objects supported by the framework are copied to the paired environment. The object will be considered equivalent to the original but not identical. The same object referenced by the object graph will result in the creation of two equivalent objects in the remote environment but they may not be identical.

## RsrService

RsrService subclasses RsrObject. In addition to the inherited behavior, RsrService includes an instance variable called #remoteSelf. #remoteSelf is a forwarder/proxy object. Message sends will be dispatched to the remote environment. The message send will appear synchronous. Network issues will result in a network exception.

Under the covers, the thread making the call will wait on a Promise object. The transaction id will be mapped to the promise. When a response is received, the promise will be fulfilled. An RsrUnansweredMessage object will be created in order to provide this mapping.

## Coordination Windows

Changes to mirrored service propagate to the remote environment during a coordination window. A window opens just before a message is sent or a response is returned.

## RsrService

RsrService provides the abstraction for creating services. The interface for object mirroring is defined on RsrService and is inherited by subclasses.

### Public Instance Variables

* #remoteSelf

### Public Interface

* #synchronize

### Private Instance Variables

* #rsrId
* #rsrConnection

### Private Interface

* #rsrId
* #rsrId:
* #rsrConnection
* #rsrConnection:

## RsrService

## Open Questions

* How should mirrors be initialized in the new environment? For instance, if a client is created and then mirrored into a bridged environment, initialization may be required to connect to the correct domain objects.
* How do we handle this case? ObjectA originates in EnvironmentA. EnvironmentB has a mirror of ObjectA. Object is not dirty. EnvironmentA sends a message to ObjectA's remoteSelf. Meanwhile, ObjectA is garbage collected and a ReleaseObjects message is sent from EnvironmentB to EnvironmentA. ObjectA no longer exists and cannot be looked up. EnvironmentA cannot handle this case in any reasonable way without a two-phase release it seems.
  * The problem really shouldn't manifest in this form. The service will send a copy of itself which will either update or create a client. The problem is that a ReleaseService message will be processed afterward. This is probably fine as it shouldn't cause issue with the existing call. Future calls may fail though.

Forking a process for each request may not be valid as it could result in an inconsistent data structure. Think a collaborating group of services entering critical code paths all at once. Create dispatcher to resolve this.

## Things to do

* Test forwarder
* Test dirty objects
* Test RsrObject

## Test Cases

### Services

1. No Cycles
	a. w/o instance variables
	b. w/ inst var referencing Service w/o instance variables
	c. w/ all data object
	d. w/ all RsrCollections
	e. same RsrCollection via two paths
2. Cycles
	a. ServiceA <-> ServiceA
	b. ServiceA <-> ServiceB
	c. ServiceA -> anRsrCollection <-> anRsrCollection
3. Sending Messages
	a. Nothing dirty in graph
	b. Dirty objects in graph
	c. Unary
	d. Binary
	e. keyword
	f. #perform: family
	g. Sending each RsrObjects
		1. Already mirrored
		2. Currently un-mirrored
	h. Returning each RsrObject
		1. Already mirrored
		2. Currently un-mirrored
	i. Dirty objects outside of current object graph are not sent
4. Encoding
	a. Encode only variables which exist between RsrService and concrete instance class.
	b. The transitive closure of a service is transferred regardless of whether the object was changed.
5. Concurrent message sends
	a. Disjointed Services w/o shared sub-graph
	b. Services w/ shared sub-graph which is dirty
6. Mirroring Objects
	a. Already mirror for current connection
	b. Already mirror for other connection (Exception?)
	c. Un-mirrored object
7. Client Service turns into Server Service

RsrStateCoordinator seems to have two phases.
	1. Discovery of dirty and newly mirrored objects + encoding individual object
	2. Calculate length + write object on wire

What is a good name for the dirty + new discovery phase?
	ChangeAnalysis? DirtyCalculator?


List of in-line Data Objects
	1. SmallInteger

Expectation:
	1. All instance variables between concrete service instance class and RsrService (exclusive) are included in order. Instance variables in RsrService and above are not mapped. Instance variables defined in instance's class are not mapped.

## Object Encodings

### RsrService Layout

```protocol --bits 32 "Type:32,OID:32,Instance Variable Count:32,Service Name Reference:32, [Object References]:32"
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                              Type                             |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                              OID                              |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                    Instance Variable Count                    |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                     Service Name Reference                    |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                       [Object References]                     |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

#### Type

The Object Type Identifier for Services.

#### OID

The object identifier assigned to the encoded service.

#### Service Name Reference

The name of the remote service to instantiate. This is encoded as an immediate symbol.

#### Object References

A series of zero or more object references. May hold an OID referencing another object or may hold an immediately encoded object.

## Data Object Encoding

Data Objects are not treated as objects in their own rights. They are always encoded as immediate values and encoded in-line in another object. An object reference of 0 is used to denote the start of a data object. The object immediately follows.

## Command Identifiers

| Command			| Identifier	|
|---				|---			|
| RetainService		| 0				|
| SendMessage		| 1				|
| DeliverResponse	| 2				|
| ReleaseServices	| 3				|

## Object Type Identifiers

| Object			| Identifier	| Notes							|
|---				|---			|---							|
| RsrService		| 0				|								|
| Symbol			| 1				| UTF-8 Encoded					|
| String			| 2				| UTF-8 Encoded 				|
| Positive Integer	| 3				| Big Endian					|
| Negative Integer	| 4				| Bit Endian					|
| Character			| 5				| Codepoint as Int 				|
| nil				| 6				| 								|
| true				| 7				|								|
| false				| 8				|								|
| Array				| 9				|								|
| ByteArray			| 10			|								|
| Set				| 11			| 								|
| OrderedCollection	| 12			|								|
| Dictionary		| 13			| key then value then key...	|
| DateTime			| 14			| Microseconds since unix epoch	|

## Symbol/String Encoding

Symbols and Strings are encoded in the same format. They only differ in the value of their type field.

| Field        		| Value 						|
|---				|---							|
| OID				| 0 to signify immediate		|
| Immediate Type	| Assigned type designation		|
| Length			| Number of UTF-8 encoded bytes	|
| Data				| String encoded using UTF-8	|

## DateTime Encoding

RSR will not support encoding Date or Time encoding. It will support encoding the equivalent of DateTime.

The value will be as a signed 64-bit integer. The value is to be interpreted as microseconds since the unix epoch.

| Field				| Value											|
|---				|---											|
| OID				| 0 to signify immediate						|
| Immediate Type	| Assigned type designation						|
| Data				| Microseconds since unix epoch (64-bit signed)	|
