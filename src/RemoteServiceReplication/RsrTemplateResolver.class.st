Class {
	#name : 'RsrTemplateResolver',
	#superclass : 'RsrAbstractTemplateResolver',
	#category : 'RemoteServiceReplication'
}

{ #category : 'resolving' }
RsrTemplateResolver >> templateFor: aService [
	"Resolve the template associated with the provided Service."

	^aService class templateClass
]

{ #category : 'resolving' }
RsrTemplateResolver >> templateNamed: aTemplateName [
	"Resolve a template with the provided name."

	^(RsrClassResolver classNamed: aTemplateName) templateClass
]
