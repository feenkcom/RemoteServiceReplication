# Remote Service Replication

To provide a means of bridging to disparate Smalltalk environments together, allowing one image to define the UI and another to hold the domain objects.

## High-level Operations

* Send message to service partner
* Replicate changes to service objects (and their object graphs? and their collections?)

## Requirements of Environment

* Object Finalization
* Weak value collections
* Concurrency model

## Types of Objects

* Data Objects
  * Number
  * String
  * TimeStamp
* RsrCollections
  * Set
  * Dictionary
  * Array
  * ByteArray

## Features of RSR

* Encode and transfer objects between environments
* Provide service abstraction allowing events to be processed locally or remotely
* Extend notion of equality across environment boundaries
* Expose network instead of trying to abstract it away.

## Low-level Message Types:

* #Object
* #Message
* #Reply
* #GarbageID -> #Release

### Object Type

Contains an Object ID and encoded references to other objects. How are the objects here released? The local object must be strongly referenced since a message may come at a later time. Once a message obtains an object, do we stop referencing that object? If so, does the remote have to transfer the object every time it wishes to reference the object? Do we swap it to a weak reference? If so, we need to deal with synchronizing the retention state with the remote side. During finalization we need to save the object until such a time that the remote acks the release of the object.

### Message Type

Contains a transaction ID, a selector and references to objects that will serve as arguments.

### Reply Type

Contains a transaction ID and a reference to the object that should serve as a response.

### Release Type

Contains (a) reference(s) to objects which are no longer strongly referenced.

## Object Lifetimes

Objects sent to remote environments are considered living until the remote environment signals their release.

Objects received from another environment must be strongly retained until consumed by an incoming message.

## Service Creation

The initial service will be registered by the framework. It's purpose will be to facilitate the creation of additional services.

## Object Serialization

The framework will serialize and transfer objects in three separate ways.

* Mirroring
* Copying the value

### Mirroring

Classes that inherit from RsrObject will mirror when sent to a bridged environment. Changes to a mirrored object will result in the change propagating to the bridged environment during a coordination window.

How do I define the parts of me that should be reflected and the parts that lack reflection?

### By Value

Immutable primitive objects will be transferred by value from one environment to the other.

## RsrService

RsrService subclasses RsrObject. In addition to the inherited behavior, RsrService includes an instance variable called #remoteSelf. #remoteSelf is a forwarder/proxy object. Message sends will be dispatched to the remote environment. The message send will appear synchronous.

Under the covers, the thread making the call will wait on a Promise object. The transaction id will be mapped to the promise. When a response is received, the promise will be fulfilled. An RsrUnansweredMessage object will be created in order to provide this mapping.

## Coordination Windows

Changes to mirrored objects propagate to the remote environment during a coordination window. A window opens just before a message or a response are sent. 

What happens if a conflicting change is detected during a coordination window? i.e. each paired service is has changed the inst var #foo between synchronization windows? Such a conflict is considered an application bug.

## RsrObject

RsrObject is the abstract class which defines the protocol used for mirroring object across environments.

### Public Interface

* #isDirty
* #isClean
* #markDirty

### Private Interface

* #rsrId
* #rsrId:
* #rsrRemoteInterface
* #rsrRemoteInterface:
* #rsrMarkClean

## RsrService

RsrService provides the abstraction for creating services. In addition to providing the RsrObject interface, it adds an unexposed #remoteSelf instance variable. #remoteSelf contains an RsrForwarder which proxies messages synchronously to the paired remote service.

## Open Questions

* How are service objects 'registered' with the system? By virtue of their appearance in the environment? They must be held onto by something.
* How should mirrors be initialized in the new environment? For instance, if a client is created and then mirrored into a bridged environment, initialization may be required to connect to the correct domain objects.
* Do all objects going to the remote environment require an object identifier? If not, how do you map the #remoteSend: message to the objects in the graph?


* How are instance variables mapped? To indices or to names?






Forwarders are not in play. There is a proxy called 'forwarder' that can be used to 'remoteSelf doIt' when in a service.

Sending all dirty objects could result in a bad state remotely if thread2 is in the process of updating while thread1 sends a message. Think GUI which is a valid reader of a data structure locally while the remote side does mutations.

RsrPromise is a thing.

Only RsrObject and data types will be transferred. Others cause an exception to be signalled.

Forking a process for each request may not be valid as it could result in an inconsistent data structure. Think a collaborating group of services entering critical code paths all at once.


## Things to do

* Test forwarder
* Test dirty objects
* Test RsrObject

## Object Encodings

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
	b. A 'clean' object is not encoded.
	c. A 'dirty' object is encoded.
	d. 'Dirty' objects referenced from a 'clean' object are encoded.
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

```protocol
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                                                             Length                                                            |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                                                              Type                                                             |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                                                              OID                                                              |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                                                          Service Name                                                         |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                                                        Object Reference                                                       |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

#### Length

The total length of the encoded object.

#### Type

The type of the encoded object.

#### OID

The object identifier assigned to the encoded object.

#### Service Name

The name of the remote service to instantiate. This is encoded as an immediate object. See Symbol/String encoding.

#### Object Reference

A series of zero or more object references. May hold an OID referencing another object or may hold an immediately encoded object.

## Data Object Encoding

Data Objects are not treated as objects in their own rights. They are always encoded as immediate values and encoded in-line in another object. An object reference of 0 is used to denote the start of a data object. The object immediately follows.

## Symbol/String Encoding

Symbols and Strings are encoded in the same format. They only differ in the value of their type field.

| Field        		| Value 						|
|---				|---							|
| Immediate Type	| Assigned type designation		|
| Length			| Number of UTF-8 encoded bytes	|
| Data				| String encoded using UTF-8	|

