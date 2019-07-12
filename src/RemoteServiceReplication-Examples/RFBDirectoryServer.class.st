Class {
	#name : #RFBDirectoryServer,
	#superclass : #RFBAbstractDirectory,
	#category : #'RemoteServiceReplication-Examples-FileBrowser'
}

{ #category : #accessing }
RFBDirectoryServer >> entities [

	true ifTrue: [^Array with: RFBFileServer new].
	^Array
		with: (RFBFileServer pathString: (self path / 'foo') pathString)
]
