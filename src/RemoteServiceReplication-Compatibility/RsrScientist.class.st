Class {
	#name : #RsrScientist,
	#superclass : #Object,
	#category : #'RemoteServiceReplication-Compatibility'
}

{ #category : #instrumenting }
RsrScientist >> instrument: aBlock
label: aString [

	| time result |
	time := Time millisecondsToRun: [result := aBlock value].
	Transcript
		show: DateAndTime now printString, ': ', aString asString, ' took (', time asString, ') ms.';
		cr.
	^result
]

{ #category : #instrumenting }
RsrScientist >> profile: aBlock
label: aString [

	| result |
	TimeProfiler onBlock: [result := aBlock value].
	^result
]

{ #category : #instrumenting }
RsrScientist >> profile: aBlock
label: aString
if: aCondition [

	| result |
	aCondition
		ifTrue: [TimeProfiler onBlock: [result := aBlock value].]
		ifFalse: [result := aBlock value].
	^result
]
