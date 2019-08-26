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
loadRSRBase
    "run by SystemUser, loads the RSR-Base package into UserGlobals."
    | repositoryRoot |
    repositoryRoot := '/home/kkilpela/development/repos/RSR-Prototype'.
    self
    loadFromRoot: repositoryRoot
    ston: 'RemoteServiceReplication-Base.ston'.
    ^true
%

! load RemoteServiceReplication-Base package into UserGlobals.
send RsrLoader loadRSRBase
