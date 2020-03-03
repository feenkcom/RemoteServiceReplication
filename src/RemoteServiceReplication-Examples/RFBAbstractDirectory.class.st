Class {
	#name : #RFBAbstractDirectory,
	#superclass : #RFBAbstractFileSystemEntity,
	#category : #'RemoteServiceReplication-Examples-FileBrowser'
}

{ #category : #accessing }
RFBAbstractDirectory class >> clientClassName [

	^#RFBDirectory
]

{ #category : #accessing }
RFBAbstractDirectory class >> serverClassName [

	^#RFBDirectoryServer
]

{ #category : #testing }
RFBAbstractDirectory >> isDirectory [

	^true
]
