# Remote Service Replication (Draft 4)

To provide a means of bridging two Smalltalk environments allowing the sharing of resources.

## High-level Operations

* Send message to service partner
* Create services
* Synchronize service and its object graph

## Requirements of Smalltalk Environment

* Object Finalization
* Concurrency model

## Object Species

* Services
* Data Objects
  * Integer
  * String
  * DateAndTime
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
* Expose network instead of trying to abstract it away. Network errors are handled by user of RSR vs handled transparently.

## Low-level Message Types:

* #RetainService
* #SendMessage
* #DeliverResponse
* #ReleaseServices

### RetainService

This message instructs the remote environment to strongly retain the embedded object until a message send or DeliverResponse is processed. Each service in the transitive closure of the message send will be encapsulated in an independence RetainService message. Data objects are encapsulated in the service or object which refer to them. Unsupported objects will result in an exception.

### SendMessage

Instruct the framework to send a message. The receiver, selector, and arguments are included in the message. The result is returned via a DeliverResponse message. Should an error occur, this should be return via a DeliverResponse containing an exception.

### DeliverResponse

Contains a message ID and a reference to the object that should serve as a response.

### ReleaseServices

When a Client is garbage collected, its identifier is placed into a ReleaseServices message. The peer handles this message by releasing its reference to the corresponding Server.

## Service Lifetimes

Services come in two variations: Client and Server. The lifetime of services is defined by their Client instance. A Server will exist for at least as long as its Client exists.

All services received from another environment must be strongly retained until consumed by an incoming message or return.

## Service Creation

A ServiceFactory service will be registered by the framework. It's purpose will be to facilitate the creation of additional services.

## Object Serialization

The framework will serialize and transfer objects in two separate ways.

* Mirroring
* Copying

### Mirroring

Classes that inherit from RsrService will mirror when sent to a bridged environment. Changes to a mirrored service will result in the change propagating to the remote environment during a coordination window.

### Copying

Supported data objects are copied to the remote environment. The object will be considered equivalent to the original but not identical. The same object referenced by the object graph will result in the creation of two equivalent objects in the remote environment but they may not be identical.

## RsrService

RsrService includes an instance variable called #remoteSelf. #remoteSelf is a forwarder/proxy object. Message sends will be dispatched to the remote environment. The message send will appear synchronous. Network issues will result in the signaling of a network exception.

Under the covers, the thread making the call will wait on a Promise object. The transaction id will be mapped to the promise. When a response is received, the promise will be fulfilled. An RsrUnansweredMessage object will be created in order to provide this mapping.

Should teh connection close, pending promises will mark as failed with an RsrConnectionClosed error.

## Coordination Windows

Changes to mirrored service propagate to the remote environment during a coordination window. A window opens before a message is sent or a response is returned.

## RsrService

RsrService provides the abstraction for creating services. The interface for object mirroring is defined on RsrService and is inherited by subclasses.

### Public Instance Variables

* #remoteSelf

### Public Interface

* #synchronize

### Private Instance Variables

* #_id
* #_connection

### Private Interface

* #_id
* #_id:
* #_connection
* #_connection:

## Object Encodings

### RsrService Layout

[comment]: # (protocol --bits 32 "Species:32,OID:32,Instance Variable Count:32,Service Name Reference:32, [Object References]:32")
```protocol
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                            Species                            |
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

#### Species

The identification field used for differentiating species.

#### OID

The object identifier assigned to the encoded service.

#### Service Name Reference

The name of the remote service to instantiate. This is encoded as an immediate symbol.

#### Object References

A series of zero or more object references. May hold an OID referencing another object or may hold an immediately encoded object.

## Data Object Encoding

Data Objects are not treated as objects in their own rights. They are always encoded as immediate values and encoded in-line in another object or command. An object reference of 0 is used to denote the start of a data object. The encoded object follows.

## Command Identifiers

| Command			| Identifier	|
|---				|---			|
| RetainService		| 0				|
| SendMessage		| 1				|
| DeliverResponse	| 2				|
| ReleaseServices	| 3				|

## Object Species Identifiers

| Object			| Identifier	| Notes							|
|---				|---			|---							|
| RsrService		| 0				|								|
| Symbol			| 1				| UTF-8 Encoded					|
| String			| 2				| UTF-8 Encoded 				|
| Positive Integer	| 3				| Big Endian					|
| Negative Integer	| 4				| Big Endian					|
| Character			| 5				| Codepoint as Int 				|
| nil				| 6				| 								|
| true				| 7				|								|
| false				| 8				|								|
| Array				| 9				|								|
| ByteArray			| 10			|								|
| Set				| 11			| 								|
| OrderedCollection	| 12			|								|
| Dictionary		| 13			| key then value then key...	|
| DateAndTime		| 14			| Microseconds since unix epoch	|

## Symbol/String Encoding

Symbols and Strings are encoded in the same format. They only differ in the value of their type field.

| Field        		| Value 						|
|---				|---							|
| OID				| 0 to signify immediate		|
| Immediate Species	| Assigned species designation	|
| Length			| Number of UTF-8 encoded bytes	|
| Data				| String encoded using UTF-8	|

## DateTime Encoding

RSR will not support encoding Date or Time independently. It will support encoding the equivalent of DateAndTime. The encoding may entail a loss of precision.

The value will be encoded as a signed 64-bit integer. The value is to be interpreted as microseconds since the unix epoch.

| Field				| Value											|
|---				|---											|
| OID				| 0 to signify immediate						|
| Immediate Type	| Assigned type designation						|
| Data				| Microseconds since unix epoch (64-bit signed)	|
