Class {
	#name : #RFBAbstractDirectory,
	#superclass : #RFBAbstractFileSystemEntity,
	#category : #'RemoteServiceReplication-Examples-FileBrowser'
}

{ #category : #accessing }
RFBAbstractDirectory class >> clientClass [

	^RFBDirectory
]

{ #category : #accessing }
RFBAbstractDirectory class >> serverClass [

	^RFBDirectoryServer
]

{ #category : #testing }
RFBAbstractDirectory >> isDirectory [

	^true
]
