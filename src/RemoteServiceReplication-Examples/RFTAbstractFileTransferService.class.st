Class {
	#name : #RFTAbstractFileTransferService,
	#superclass : #RsrService,
	#category : #'RemoteServiceReplication-Examples-FileTransfer'
}

{ #category : #accessing }
RFTAbstractFileTransferService class >> clientClassName [

	^#RFTFileTransferClient
]

{ #category : #accessing }
RFTAbstractFileTransferService class >> serverClassName [

	^#RFTFileTransferServer
]
