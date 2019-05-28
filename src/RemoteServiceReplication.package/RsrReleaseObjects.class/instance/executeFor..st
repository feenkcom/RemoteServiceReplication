executing
executeFor: aConnection

	| registry |
	Transcript
		show: 'Releasing: ', oids printString;
		cr.
	registry := aConnection registry.
	oids do: [:oid | registry removeKey: oid]