run
Object
	subclass: #RsrLoader
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

classmethod: RsrLoader
loadFromRoot: repositoryRoot ston: aStonFileName 
  | platformConfigurationAttributes specUrl projectDefinition |
  SessionTemps current at: #ROWAN_TRACE put: #gciLogServer .
  platformConfigurationAttributes := { 'common'. 'core'. 'gemstone' }.
  specUrl := repositoryRoot asFileReference /'rowan'/ 'specs' / aStonFileName .
  projectDefinition := RwComponentProjectDefinition newForUrl: 'file://', (specUrl pathString).
  projectDefinition
    repositoryRoot: repositoryRoot ;
    packageConvention: 'Rowan' . "to match rowan/src/properties.st"

  projectDefinition load: platformConfigurationAttributes.
%

classmethod: RsrLoader
load: aPackageName

    | repositoryRoot |
    repositoryRoot := System gemEnvironmentVariable: 'RSR_ROOT'.
    repositoryRoot ifNil: [Error signal: 'RSR_ROOT must be defined in environment'].
    [self
        loadFromRoot: repositoryRoot
        ston: aPackageName, '.ston']
      on: LookupError
      do: [:ex | ex isResumable ifTrue: [ex resume] "Avoids informational error from stopping topaz"].
    ^true
%