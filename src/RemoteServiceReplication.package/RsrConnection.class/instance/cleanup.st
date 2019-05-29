lifecycle
cleanup
	objectCache := serviceRegistry := registry := nil	"Since the connection is used in finalization, unless we clear the cache
		objects which are in the cache will never finalize. The cache is required in order to 
		ensure an object exists long enough for the upcoming message send or deliver response
		to be processed.
		This will need to change if we support resuming the connection."