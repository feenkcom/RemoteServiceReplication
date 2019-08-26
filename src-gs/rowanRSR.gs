classmethod: RsrLoader
loadRSR
    "run by SystemUser, loads the RSR-Base package into UserGlobals."
    | repositoryRoot |
    repositoryRoot := '/home/kkilpela/development/repos/RSR-Prototype'.
    self
        loadFromRoot: repositoryRoot
        ston: 'RemoteServiceReplication.ston'.
    ^true
%

! load RemoteServiceReplication package into UserGlobals.
send RsrLoader loadRSR
