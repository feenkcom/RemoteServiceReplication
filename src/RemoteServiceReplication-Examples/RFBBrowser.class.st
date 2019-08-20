Class {
	#name : #RFBBrowser,
	#superclass : #Object,
	#instVars : [
		'root'
	],
	#category : #'RemoteServiceReplication-Examples-FileBrowser'
}

{ #category : #opening }
RFBBrowser class >> openOn: aDirectory [

	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow:
			[:a |
			a tree
				title: 'No expansion';
				children: [:x :i | x isDirectory ifTrue: [x entities asOrderedCollection] ifFalse: [OrderedCollection new]];
		selectionAct:
			[:presentation | | rFile |
			rFile := presentation selection.
			rFile transferClient storeAt: (FileLocator home asPath / 'Downloads' / rFile name)]
		entitled: 'Download...'
		enabledWhen: [:presentation | presentation selection isFile]].
	^ browser openOn: (OrderedCollection with: aDirectory)
]

{ #category : #opening }
RFBBrowser class >> sample [
	"self sample"

	| socketPair connectionA connectionB |
	socketPair := RsrSocketPair new.
	connectionA := RsrConnection
		socket: socketPair firstSocket
		transactionSpigot: RsrThreadSafeNumericSpigot naturals
		oidSpigot: (RsrThreadSafeNumericSpigot start: 2 step: 1).
	connectionB := RsrConnection
		socket: socketPair secondSocket
		transactionSpigot: RsrThreadSafeNumericSpigot naturals negated
		oidSpigot: (RsrThreadSafeNumericSpigot start: 2 step: 1) negated.
	[connectionA start] fork.
	[connectionB start] fork.
	connectionA inspect.
	connectionB inspect.
	self openOn: ((connectionA serviceFor: #RsrEvaluationClient) evaluate: 'RFBDirectoryServer pathString: ''/''')

]
