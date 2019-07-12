Class {
	#name : #RFBAbstractFileSystemEntity,
	#superclass : #RsrService,
	#instVars : [
		'pathString'
	],
	#category : #'RemoteServiceReplication-Examples-FileBrowser'
}

{ #category : #'instance creation' }
RFBAbstractFileSystemEntity class >> pathString: aPathString [

	^self new
		pathString: aPathString;
		yourself
]

{ #category : #testing }
RFBAbstractFileSystemEntity >> isDirectory [

	^false
]

{ #category : #testing }
RFBAbstractFileSystemEntity >> isFile [

	^false
]

{ #category : #accessing }
RFBAbstractFileSystemEntity >> path [

	^Path from: self pathString
]

{ #category : #accessing }
RFBAbstractFileSystemEntity >> pathString [
	^ pathString
]

{ #category : #accessing }
RFBAbstractFileSystemEntity >> pathString: anObject [
	pathString := anObject
]
