Class {
	#name : #BaselineOfRemoteServiceReplication,
	#superclass : #BaselineOf,
	#category : #BaselineOfRemoteServiceReplication
}

{ #category : #baselines }
BaselineOfRemoteServiceReplication >> baseline: spec [
	<baseline>
	spec
		for: #common
			do: [ spec
				package: 'RemoteServiceReplication-Base';
				package: 'RemoteServiceReplication-Examples'
					with: [ spec requires: #('RemoteServiceReplication') ];
				package: 'RemoteServiceReplicationCompatibility-Test'
					with: [ spec requires: #('RemoteServiceReplication') ];
				package: 'RemoteServiceReplication-Test'
					with: [ spec requires: #('RemoteServiceReplicationCompatibility-Test') ] ];
		for: #pharo
			do: [ spec
				package: 'RemoteServiceReplication-Compatibility-Pharo'
					with: [ spec requires: #('RemoteServiceReplication-Base') ];
				package: 'RemoteServiceReplication'
					with: [ spec requires: #('RemoteServiceReplication-Compatibility-Pharo') ] ].
	spec
		group: 'default' with: #('All');
		group: 'All'
			with: #('RemoteServiceReplication-Test' 'RemoteServiceReplication-Examples');
		group: 'Core' with: #('RemoteServiceReplication');
		group: 'Tests' with: #('RemoteServiceReplication-Test')
]
