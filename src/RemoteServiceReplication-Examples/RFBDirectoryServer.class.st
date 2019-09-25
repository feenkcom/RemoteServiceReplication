Class {
	#name : #RFBDirectoryServer,
	#superclass : #RFBAbstractDirectory,
	#category : #'RemoteServiceReplication-Examples-FileBrowser'
}

{ #category : #accessing }
RFBDirectoryServer >> entities [

	^self instruments
		profile:
			[[self path asFileReference children sorted
				collect:
					[:each | | childPathString |
					childPathString := each asPath pathString.
					each isDirectory
						ifTrue: [RFBDirectoryServer pathString: childPathString]
						ifFalse: [RFBFileServer pathString: childPathString]]]
				on: Error
				do: [:ex | Transcript show: ex printString; cr. ex return: OrderedCollection new]]
		label: 'Enumerating ', self pathString, ' entities'
		if: self pathString = '/nix/store'
]
