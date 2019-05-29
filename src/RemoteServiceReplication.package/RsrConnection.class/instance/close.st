lifecycle
close

	isOpen
		ifFalse: [^self].
	isOpen := false.
	commandReader stop.
	commandWriter stop.
	self promises do: [:each | each error: (RsrConnectionClosed new)].
	"Since the connection is used in finalization, unless we clear the cache
		objects which are in the cache will never finalize. The cache is required in order to 
		ensure an object exists long enough for the upcoming message send or deliver response
		to be processed.
		This will need to change if we support resuming the connection."
	objectCache := serviceRegistry := registry := nil