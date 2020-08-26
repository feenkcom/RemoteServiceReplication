Class {
	#name : 'RsrValueReference',
	#superclass : 'RsrImmediateReference',
	#instVars : [
		'value'
	],
	#category : 'RemoteServiceReplication'
}

{ #category : 'resolving' }
RsrValueReference >> resolve: aRegistry [

	^value
]
