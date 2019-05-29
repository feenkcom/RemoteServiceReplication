executing
executeFor: aConnection

	| registry |
	registry := aConnection registry.
	oids do: [:oid | registry removeKey: oid]