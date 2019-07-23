Class {
	#name : #RFTFileTransferClient,
	#superclass : #RFTAbstractFileTransferService,
	#category : #'RemoteServiceReplication-Examples-FileTransfer'
}

{ #category : #accessing }
RFTFileTransferClient >> contents [

	^remoteSelf contents
]

{ #category : #accessing }
RFTFileTransferClient >> storeAt: aFilePath [

	| stream |
	stream := aFilePath asFileReference binaryWriteStream.
	[stream nextPutAll: self contents]
		ensure: [stream flush; close]
]
