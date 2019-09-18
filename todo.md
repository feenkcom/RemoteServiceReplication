## Dolphin Port

- [x] Implement compatibility subsystem
    - [x] Add seam for RsrService finalization hook (via RsrAbstractService)
- [] #testRemoteReferenceLifetime failure
- [] TimeStamp does not support timezones
    - [] Test using Dolphin 7.1.14 - DateAndTime implemented

## Needed Tests

- [] Ensure reflected instance variables appear in remoteSelf
- [] Codify rules for reflected/unreflected instance variables with tests
    - [] Reflected instance variables appear in remoteSelf
    - [] Instance variables in concrete classes do not appear
    - [] Instance variables declared in RsrService and above are not mirrored

## Needed Functionality

- [] Stream Decorator (implementing Stream interface)
    - [] RsrBufferingSocketStream
