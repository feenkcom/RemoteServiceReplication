Class {
	#name : #RsrCodec,
	#superclass : #RsrObject,
	#category : #'RemoteServiceReplication-Communications'
}

{ #category : #'private-accessing-object types' }
RsrCodec >> arrayType [

	^9
]

{ #category : #'private-accessing-object types' }
RsrCodec >> byteArrayType [

	^10
]

{ #category : #'private-accessing-object types' }
RsrCodec >> characterType [

	^5
]

{ #category : #'private-accessing' }
RsrCodec >> controlWordMax [

	^(2 raisedTo: 63) - 1
]

{ #category : #'private-accessing' }
RsrCodec >> controlWordMin [

	^(2 raisedTo: 63) negated
]

{ #category : #'private-accessing-object types' }
RsrCodec >> dateAndTimeType [

	^14
]

{ #category : #'private-accessing-commands' }
RsrCodec >> deliverResponseCommand [

	^2
]

{ #category : #'private-accessing-object types' }
RsrCodec >> dictionaryType [

	^13
]

{ #category : #'private-accessing-object types' }
RsrCodec >> falseType [

	^8
]

{ #category : #'private-testing' }
RsrCodec >> isImmediate: anObject [

	^self speciesMapping includesKey: anObject class
]

{ #category : #'private-accessing-object types' }
RsrCodec >> negativeIntegerType [

	^4
]

{ #category : #'private-accessing-object types' }
RsrCodec >> nilType [

	^6
]

{ #category : #'private-accessing-object types' }
RsrCodec >> orderedCollectionType [

	^12
]

{ #category : #'private-accessing-object types' }
RsrCodec >> positiveIntegerType [

	^3
]

{ #category : #'private-accessing-commands' }
RsrCodec >> releaseObjectsCommand [

	^3
]

{ #category : #'private-accessing-commands' }
RsrCodec >> retainObjectCommand [

	^0
]

{ #category : #'private-accessing-commands' }
RsrCodec >> sendMessageCommand [

	^1
]

{ #category : #'private-accessing-object types' }
RsrCodec >> serviceType [

	^0
]

{ #category : #'private-accessing-object types' }
RsrCodec >> setType [

	^11
]

{ #category : #'private-accessing' }
RsrCodec >> sizeOfInteger [
	"Return the number of bytes used to encode an integer"

	^8
]

{ #category : #'private-accessing' }
RsrCodec >> speciesMapping [
	"Return a mapping between the native class and their associated RsrSpecies"

	^RsrSpecies speciesMapping
]

{ #category : #'private-accessing-object types' }
RsrCodec >> stringType [

	^2
]

{ #category : #'private-accessing-object types' }
RsrCodec >> symbolType [

	^1
]

{ #category : #'private-accessing-object types' }
RsrCodec >> trueType [

	^7
]
