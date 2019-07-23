Class {
	#name : #RFTFileTransferServer,
	#superclass : #RFTAbstractFileTransferService,
	#instVars : [
		'path'
	],
	#category : #'RemoteServiceReplication-Examples-FileTransfer'
}

{ #category : #'instance creation' }
RFTFileTransferServer class >> path: aFilePath [

	^self new
		path: aFilePath;
		yourself
]

{ #category : #accessing }
RFTFileTransferServer >> contents [

	| stream |
	stream := self path asFileReference binaryReadStream.
	^[stream contents]
		ensure: [stream close]
]

{ #category : #accessing }
RFTFileTransferServer >> path [

	^path
]

{ #category : #accessing }
RFTFileTransferServer >> path: aFilePath [

	path := aFilePath
]
