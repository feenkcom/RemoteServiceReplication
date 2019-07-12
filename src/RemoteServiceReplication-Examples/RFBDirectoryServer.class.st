Class {
	#name : #RFBDirectoryServer,
	#superclass : #RFBAbstractDirectory,
	#category : #'RemoteServiceReplication-Examples-FileBrowser'
}

{ #category : #accessing }
RFBDirectoryServer >> entities [

	^[self path asFileReference children sorted
		select: [:each | each exists]
		thenCollect:
			[:each | | childPathString |
			childPathString := each asPath pathString.
			each isDirectory
				ifTrue: [RFBDirectoryServer pathString: childPathString]
				ifFalse: [RFBFileServer pathString: childPathString]]]
		on: Error
		do: [:ex | Transcript show: ex printString; cr. ex return: OrderedCollection new]
]
