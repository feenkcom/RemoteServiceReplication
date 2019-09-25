Class {
	#name : #RFBDirectory,
	#superclass : #RFBAbstractDirectory,
	#category : #'RemoteServiceReplication-Examples-FileBrowser'
}

{ #category : #accessing }
RFBDirectory >> entities [

	^self instruments
		instrument: [remoteSelf entities]
		label: 'Request ', self pathString, ' entities'
]
