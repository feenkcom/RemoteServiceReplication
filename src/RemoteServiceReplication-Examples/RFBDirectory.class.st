Class {
	#name : #RFBDirectory,
	#superclass : #RFBAbstractDirectory,
	#category : #'RemoteServiceReplication-Examples-FileBrowser'
}

{ #category : #accessing }
RFBDirectory >> entities [

	^remoteSelf entities
]
