instance creation
stream: aReadStream
decoder: anObjectDecoder
connection: aConnection

	^self new
		stream: aReadStream;
		decoder: anObjectDecoder;
		connection: aConnection;
		yourself