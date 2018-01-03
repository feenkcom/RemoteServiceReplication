as yet unclassified
send: anObject to: aRsrServer
	|host socket objectToSend address rsrBroker results|
	rsrBroker := RsrBroker new.
	address := aRsrServer hostname.
	port := aRsrServer port.
	socket := Socket newTCP.
	host := NetNameResolver addressForName: address.
	results := nil.

	objectToSend  := rsrBroker prepare: anObject.
	
	[ socket
			connectTo: host port: port;
			waitForConnectionFor: 10.
		socket sendData: objectToSend. 
		results := socket receiveDataTimeout: 10.
	] ensure: [ socket closeAndDestroy. ].

	Transcript show: results.

	^results
	