Class {
	#name : #RFBAbstractFile,
	#superclass : #RFBAbstractFileSystemEntity,
	#category : #'RemoteServiceReplication-Examples-FileBrowser'
}

{ #category : #accessing }
RFBAbstractFile class >> clientClass [

	^RFBFile
]

{ #category : #accessing }
RFBAbstractFile class >> serverClass [

	^RFBFileServer
]

{ #category : #testing }
RFBAbstractFile >> isFile [

	^true
]
