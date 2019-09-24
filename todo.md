## Dolphin Port

- [x] Implement compatibility subsystem
    - [x] Add seam for RsrService finalization hook (via RsrAbstractService)
- [] #testRemoteReferenceLifetime failure
    - [x] Failure of service synchronization fixed via sending _synchronize
    - [x] Failure to send ReleaseService message fixed via finalization sending #reap:
- [x] TimeStamp
    - [x] Does not support timezones (will be ignored)
    - [x] Does not support #fromString: testing format (will be ignored)
    - [x] Test using Dolphin 7.1.14 - DateAndTime implemented

## Needed Tests

- [x] Ensure reflected instance variables appear in remoteSelf
- [x] Codify rules for reflected/unreflected instance variables with tests
    - [x] Reflected instance variables appear in remoteSelf
    - [x] Instance variables in concrete classes do not appear
    - [x] Instance variables declared in RsrService and above are not mirrored
- [] RsrSocketStream
    - [] #nextAvailable
- [] RsrSocket
    - [] Data available?
    - [] Read available data
- [] Unicode
    - [] Strings containing
        - [] 8-bit characters
        - [] 16-bit characters
        - [] 32-bit characters
    - [] Symbols containing
        - [] 8-bit characters
        - [] 16-bit characters
        - [] 32-bit characters
    - [] Characters
        - [] Ascii
        - [] Extended ascii
        - [] 8-bit characters
        - [] 16-bit characters
        - [] 32-bit characters
- [] Service
    - [] Mirrored instance variable reference unsupported object

## Needed Functionality

- [] Stream Decorator (implementing Stream interface)
    - [] RsrBufferingSocketStream

## Bugs

- [] All RsrCommandProcessors should #stop regardless of whether they are waiting on a socket or a queue