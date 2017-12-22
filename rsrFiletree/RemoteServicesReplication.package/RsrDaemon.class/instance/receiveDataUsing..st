accessing
receiveDataUsing: aSocket
	|data rsrBroker|
	rsrBroker := RsrBroker new.
	[ data := aSocket receiveDataTimeout: self timeout.
	  rsrBroker receive: data.
	] ensure: [ 
	  aSocket closeAndDestroy. ].
	