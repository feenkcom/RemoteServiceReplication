running
testReturnArgument

	| client server arguments |
	client := RsrClientNoInstVars new.
	self serviceRegistryA
		at: 'TestService'
		put: client.
	server := self serviceRegistryB at: 'TestService'.
	arguments := OrderedCollection new
		addAll: #( #symbol 'string' $h 0 -14 14 18446744073709551616 -18446744073709551616 nil true false );
 		add: (Character codePoint: 16r259F);
		yourself.
	arguments
		do:
			[:each | | result |
			result := client sendReturnArgument: each.
			self
				assert: result
				equals: each].
	arguments
		do:
			[:each | | result |
			result := server sendReturnArgument: each.
			self
				assert: result
				equals: each].
	self
		assert: (client sendReturnArgument: client)
		identicalTo: client.
	self
		assert: (server sendReturnArgument: server)
		identicalTo: server