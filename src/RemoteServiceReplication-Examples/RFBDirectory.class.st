Class {
	#name : #RFBDirectory,
	#superclass : #RFBAbstractDirectory,
	#category : #'RemoteServiceReplication-Examples-FileBrowser'
}

{ #category : #accessing }
RFBDirectory >> entities [

	^self scientist
		instrument: [remoteSelf entities]
		label: 'Request ', self pathString, ' entities'
]
