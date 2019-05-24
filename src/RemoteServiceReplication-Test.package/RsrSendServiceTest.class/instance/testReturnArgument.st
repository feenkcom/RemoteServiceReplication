running
testReturnArgument

	| service remoteService literals arguments |
	service := RsrClientNoInstVars new.
	connectionA registry addLocal: service.
	service rsrRemoteInterface: connectionA remoteInterface.
	remoteService := RsrServerNoInstVars
		rsrId: service rsrId
		rsrRemoteInterface: connectionB remoteInterface.
	connectionB registry addRemote: remoteService.
	literals := #( #symbol 'string' $h 0 -14 14 18446744073709551616 -18446744073709551616 nil true false ).
	arguments := literals , (Array with: (Character codePoint: 16r259F) with: service).
	arguments
		do:
			[:each | | result |
			result := service returnArgument: each.
			self
				assert: result
				equals: each]