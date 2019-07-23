Class {
	#name : #RFBFile,
	#superclass : #RFBAbstractFile,
	#category : #'RemoteServiceReplication-Examples-FileBrowser'
}

{ #category : #accessing }
RFBFile >> transferClient [

	^remoteSelf transferServer
]
