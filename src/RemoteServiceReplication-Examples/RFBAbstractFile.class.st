Class {
	#name : #RFBAbstractFile,
	#superclass : #RFBAbstractFileSystemEntity,
	#category : #'RemoteServiceReplication-Examples-FileBrowser'
}

{ #category : #accessing }
RFBAbstractFile class >> clientClassName [

	^#RFBFile
]

{ #category : #accessing }
RFBAbstractFile class >> serverClassName [

	^#RFBFileServer
]

{ #category : #testing }
RFBAbstractFile >> isFile [

	^true
]
