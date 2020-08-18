# RsrSocket

This document describes the interface and associated semantics of RsrSocket.

## High-level Interfaces

* #bindAddress:port:
* #listen:
* #connectToHost:port:
* #accept
* #close
* #isConnected
* #read:
* #write:

### #bindAddress:port:

*address* - A string representing the local IP address used for binding.
*port* - A integer representing the local port used for binding.

The socket will bind to the provided address and port. Signals RsrInvalidBind in the event the attempted bind fails.

### #listen:

*backlogLength* - The length of the pending connections backlog.

The socket will start listening for connections. The argument specifies the length of the connection backlog. If you fail to call #bindAddress:port:, RsrInvalidListen will be signaled. The backlog may silently reduce to the implementation's maximum defined backlog length.

### #connectToHost:port:

*host* - A string representing IP address or host name.
*port* - An integer representing the port used for connection on the provided host.

Establish a connection to the provided host and port. If the socket is unable to establish, RsrConnectFailed will be signaled. If a socket is bound to a local address or is listening for connections, RsrInvalidConnect will be signaled.

### #accept

Returns an instance of RsrSocket connected to a peer. In the event that the socket is closed, RsrSocketClosed is signaled. 

### #close

Closes a socket. The resources associated with the socket are cleaned up.

### #isConnected

Returns true is a socket is currently connected to a peer. Returns false otherwise.

### #read:into:startingAt:

*count* - An integer count of bytes to read.
*bytes* - A ByteArray used to store the read bytes.
*index* - The index that should be used for the first read byte.

Read *count* bytes from the underlying socket. Return the number of bytes successfully read. The first read byte should be written into the position indicated by *index*. *bytes* should be of sufficient size to fix the requested data. If *bytes* is not of sufficient size, the behavior is undefined. If the socket is not connected, signal RsrSocketClosed.

### #write:count:startingAt:

*bytes* - The ByteArray containing the bytes that should be written.

*count* - The number of bytes that should be written.

*index* - The index of the first byte to be written.

Write *bytes* to the underlying socket. The first byte is indicated by *index*. No more than *count* bytes will be written. If *bytes* is smaller than *index + count* would require, the behavior is undefined. If the socket is not connected, signal RsrSocketClosed.
