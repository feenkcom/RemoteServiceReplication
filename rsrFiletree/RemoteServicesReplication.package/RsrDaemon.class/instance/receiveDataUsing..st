accessing
receiveDataUsing: aSocket
	|data rsrBroker results|
	rsrBroker := RsrBroker new.
	[ data := aSocket receiveDataTimeout: self timeout.
	  results := rsrBroker receive: data.
		Transcript show: 'returning data'.
		aSocket sendData: results.
	] ensure: [ 
	  aSocket closeAndDestroy ].
	