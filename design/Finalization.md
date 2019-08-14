# Object Finalization Abstraction

RSR makes assertions about the lifetimes of Services. These must be satisfied in all supported environments.

## Service Lifetime Assertions

* Clients strongly reference their Servers
* Servers are strongly retained by the framework
* Clients are weekly retained by the framework
* Services referenced by RetainObject messages are retained until a SendMessage or a DeliverResponse command is processed
* When Clients are no longer strongly referenced, the framework must alert its peer so the corresponding server can be marked as garbage

## Distinct Actions

* Weakly retain services (+ strong reference to Servers)
* Signal release of Server during Client GC

## Abstract Interface

Should the registry have responsibility for this? When a Client or Server are added, should it handle this logic?

Registry onReap: [:anObject | self release: anObject rsrId]

Special care will need to be given to the processing of an object to ensure no memory leaks occur.