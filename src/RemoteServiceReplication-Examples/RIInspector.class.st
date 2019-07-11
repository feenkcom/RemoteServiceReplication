Class {
	#name : #RIInspector,
	#superclass : #Object,
	#category : #'RemoteServiceReplication-Examples-Inspector'
}

{ #category : #'instance creation' }
RIInspector class >> openOn: anInspectorClient [

	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: [ :c | 
					c
						row: #two;
						row: #three ].
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					(a tree)
						title: 'Tree';
						children: [ :item :x :level | #() ]	"Children must return a collection" ].
	(browser transmit)
		to: #two;
		from: #one;
		andShow: [ :a | a text title: 'Selection preview' ].
	(browser transmit)
		to: #three;
		from: #one port: #selectionPath;
		andShow: [ :a | a text title: 'Selection path preview' ].
	^ browser openOn: (Array with: anInspectorClient)
]
