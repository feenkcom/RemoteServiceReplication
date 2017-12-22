as yet unclassified
send: anObject to: aRsrServer
	|host socket objectToSend address rsrBroker|
	rsrBroker := RsrBroker new.
	address := aRsrServer hostname.
	port := aRsrServer port.
	socket := Socket newTCP.
	host := NetNameResolver addressForName: address.

	objectToSend  := rsrBroker prepare: anObject.
	[ socket
		connectTo: host port: port;
		waitForConnectionFor: 10.
		socket sendData: objectToSend. 
	] ensure: [ socket closeAndDestroy  ].
	