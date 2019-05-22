accessing
dumpStack

	| stack |
	stack := String streamContents: [:stream2 | thisContext debugStackOn: stream2].
	Transcript
		show: stack;
		cr