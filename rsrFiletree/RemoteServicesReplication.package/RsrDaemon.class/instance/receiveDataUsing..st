accessing
receiveDataUsing: aSocket
	|data rsrBroker results|
	rsrBroker := RsrBroker new.
	[ data := aSocket receiveDataTimeout: self timeout.
	  results := rsrBroker deserializeAndProcess: data.
	  results ifNotNil: [ |serializedData|
		serializedData := rsrBroker serialize: results.
		aSocket sendData: serializedData.
	  ]
	] ensure: [ 
	  aSocket closeAndDestroy ].
	