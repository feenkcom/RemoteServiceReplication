Class {
	#name : #RFTAbstractFileTransferService,
	#superclass : #RsrService,
	#category : #'RemoteServiceReplication-Examples-FileTransfer'
}

{ #category : #accessing }
RFTAbstractFileTransferService class >> clientClass [

	^RFTFileTransferClient
]

{ #category : #accessing }
RFTAbstractFileTransferService class >> serverClass [

	^RFTFileTransferServer
]
