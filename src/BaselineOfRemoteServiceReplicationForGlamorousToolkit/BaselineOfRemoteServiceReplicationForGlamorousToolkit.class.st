Class {
	#name : 'BaselineOfRemoteServiceReplicationForGlamorousToolkit',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfRemoteServiceReplicationForGlamorousToolkit'
}

{ #category : 'baselines' }
BaselineOfRemoteServiceReplicationForGlamorousToolkit >> baseline: spec [
	<baseline>
	spec
		for: #common
			do: [ 
				spec
					package: 'RemoteServiceReplication-Base';
					package: 'RemoteServiceReplication-Platform-Test'
						with: [ spec requires: #('RemoteServiceReplication') ];
					package: 'RemoteServiceReplication-Test'
						with: [ spec requires: #('RemoteServiceReplication-Platform-Test') ].
		 	spec
					package: 'TonelFixes';
					package: 'RemoteServiceReplication-Pharo'
						with: [ spec requires: #('TonelFixes' 'RemoteServiceReplication-Base') ];
					package: 'RemoteServiceReplication'
						with: [ spec requires: #('RemoteServiceReplication-Pharo') ] ]
]

{ #category : 'accessing' }
BaselineOfRemoteServiceReplicationForGlamorousToolkit >> projectClass [
	Smalltalk at: #'MetacelloCypressBaselineProject' ifPresent: [ :cl | ^ cl ].
	^ super projectClass
]
