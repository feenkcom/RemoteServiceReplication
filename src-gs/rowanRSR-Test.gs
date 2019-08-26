classmethod: RsrLoader
loadRSRTest
    "run by SystemUser, loads the RSR-Base package into UserGlobals."
    | repositoryRoot |
    repositoryRoot := '/home/kkilpela/development/repos/RSR-Prototype'.
    self
        loadFromRoot: repositoryRoot
        ston: 'RemoteServiceReplication-Test.ston'.
    ^true
%

! load RemoteServiceReplication package into UserGlobals.
send RsrLoader loadRSRTest
