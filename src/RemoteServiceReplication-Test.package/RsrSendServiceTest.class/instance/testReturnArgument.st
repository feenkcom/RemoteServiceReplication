running
testReturnArgument

	| service remoteService arguments |
	service := RsrClientNoInstVars new.
	connectionA registry retain: service.
	service rsrConnection: connectionA.
	service synchronize.
	remoteService := connectionB registry at: service rsrId.
	connectionB registry remember: remoteService.
	arguments := OrderedCollection new
		addAll: #( #symbol 'string' $h 0 -14 14 18446744073709551616 -18446744073709551616 nil true false );
 		add: (Character codePoint: 16r259F);
		yourself.
	arguments
		do:
			[:each | | result |
			result := service sendReturnArgument: each.
			self
				assert: result
				equals: each].
	arguments
		do:
			[:each | | result |
			result := remoteService sendReturnArgument: each.
			self
				assert: result
				equals: each].
	self
		assert: (service sendReturnArgument: service)
		identicalTo: service.
	self
		assert: (remoteService sendReturnArgument: remoteService)
		identicalTo: remoteService