Class {
	#name : #RFBFileServer,
	#superclass : #RFBAbstractFile,
	#category : #'RemoteServiceReplication-Examples-FileBrowser'
}

{ #category : #accessing }
RFBFileServer >> transferServer [

	^RFTFileTransferServer path: self path
]
