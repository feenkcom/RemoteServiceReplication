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
    repositoryRoot := '/home/kkilpela/development/repos/RSR-Prototype'.
    self
        loadFromRoot: repositoryRoot
        ston: aPackageName, '.ston'.
    ^true
%