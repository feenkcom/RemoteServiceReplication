Class {
	#name : 'RsrAbstractTemplateResolver',
	#superclass : 'RsrObject',
	#category : 'RemoteServiceReplication'
}

{ #category : 'resolving' }
RsrAbstractTemplateResolver >> templateFor: aService [
	"Resolve the template associated with the provided Service."

	self subclassResponsibility
]

{ #category : 'resolving' }
RsrAbstractTemplateResolver >> templateNamed: aTemplateName [
	"Resolve a template with the provided name."

	self subclassResponsibility
]
