# Brainstorming

## Open Questions

- If we allow services to share a single dispatch context, is a corresponding context shared on the peer end of the connection?
    - Are dispatch contexts own by a service or the framework?
    - Are they owned by all who use them?
    - Are context 'reference counted'?
- When should services be updated? A service could be in a critical section while the framework updates it leaving it inconsistent.
    - Example: Service has entered a critical section locking access to its variables. The frameworks is assembling the service because was part of a concurrent transitive closure. During the process, the framework would update locked instance variables.
    - How would we assemble a service which transitively closes with a service that has its own dispatch queue?
    - If we assemble a service in its own context, what happens if the receiver (contextA) references a service (contextB) which then references a service (contextA)? Would we deadlock during assembly?
- If the connection drops, should RSR attempt to reconnect to its peer?
    - What protocol extensions do we require to enable this? Partial writes may have occurred.
- Does RSR shutdown cleanly?
    - On #stop
        - Do all services get notified?
        - Do we close the socket?
        - Do we shutdown all dispatch contexts?
        - Do we shutdown socket reader/writer + dispatcher?
    - On SocketError
        - Do we signal to Connection that the Socket is having an error?
- Sockets
    - Is our Socket wrapper sufficient?
    - Do we handle partial writes?
    - Streams
        - Should we buffer the stream?
        - What happens to data when it is in an internal buffer and the socket closes?
- Should we require a service is created in the context of a connection?
- Concurrency
    - Should services live in a dispatch group?

## RSR Framework Questions

RSR is a framework that allows you to extend computation between smalltalk environments. You can write your UI in Pharo and your data model in GemStone. You define Services which allow you to send messages between Smalltalk Systems.

- How does RSR 'extend computation'?
- What restrictions are in-place for messages sent using the framework?
- How does a Service differ from a proxy? From a RESTful Service?
- What additional considerations must I make when creating and using services?
- How do I define a service?


## Use Cases

- Provide Pharo GUI for model written in GemStone. (Original use case)

## Stress Test Ideas

- Chatty services
    - Send small message to remoteSelf repeatedly
    - Send large message to remoteSelf repeatedly
    - Concurrent messages
        - Single Service from multiple processes
        - Multiple Services from multiple processes
- File Transfer Services
    - Send large files between environments in chunks
- Service HTTP requests via RSR
- Latency/Throughput
    - Test the latency of a single unary message without arguments are inst vars
    - Test latency of sending ByteArray of increasing size
        - Does this scale linearly?
