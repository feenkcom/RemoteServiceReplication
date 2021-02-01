# Remote Service Replication

## Running Tests via Command Line

### Prerequisites

* `ROWAN_PROJECTS_HOME` must be set to the parent directory of the location RemoteServiceReplication's repository is cloned.

### Pharo

```
source scripts/setup-testing-environment
pharo-tests
```

### GemStone

You must first set the following environment variables. They should be set to the full path to appropriate login scripts.

* `SYSTEM_USER_LOGIN` - Performs a GemSTone login using the `SystemUser` user.
* `DATA_CURATOR_LOGIN` - Performs a GemStone login using the `DataCurator` user.

```
source scripts/setup-testing-environment
gemstone-tests
```