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
	browser transmit to: #one; andShow: [:a |
		a tree
			title: 'No expansion';
			children: [:x :i | x isDirectory ifTrue: [x entities asOrderedCollection] ifFalse: [OrderedCollection new]].
		a tree
			title: 'All expanded';
			allExpanded;
			children: [:x :i | x asString size > 1 ifTrue: [x] ifFalse: [OrderedCollection new] ] ].
	^ browser openOn: (OrderedCollection with: aDirectory)
]

{ #category : #opening }
RFBBrowser class >> sample [
	"self sample"

	| port connectionAPromise connectionBPromise connectionA connectionB |
	port := 7678.
	connectionAPromise := [RsrConnection acceptOn: port] async.
	connectionBPromise := [RsrConnection connectTo: '127.0.0.1' port: port] async.
	connectionA := connectionAPromise value.
	connectionB := connectionBPromise value.
	connectionA inspect.
	connectionB inspect.
	self openOn: ((connectionA serviceFor: #RsrEvaluationClient) evaluate: 'RFBDirectoryServer pathString: ''/''')

]
