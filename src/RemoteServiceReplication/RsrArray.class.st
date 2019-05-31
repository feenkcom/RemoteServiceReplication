Class {
	#name : #RsrArray,
	#superclass : #RsrCollection,
	#type : #variable,
	#category : 'RemoteServiceReplication-Public'
}

{ #category : #enumerating }
RsrArray >> do: aBlock [

	1 to: self size do: [:i | aBlock value: (self at: i)]
]
