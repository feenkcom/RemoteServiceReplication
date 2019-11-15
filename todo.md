## Dolphin Port (7.0.56)

- [x] Implement compatibility subsystem
    - [x] Add seam for RsrService finalization hook (via RsrAbstractService)
- [x] #testRemoteReferenceLifetime failure
    - [x] Failure of service synchronization fixed via sending _synchronize
    - [x] Failure to send ReleaseService message fixed via finalization sending #reap:
- [x] TimeStamp
    - [x] Does not support timezones (will be ignored)
    - [x] Does not support #fromString: testing format (will be ignored)
    - [x] Test using Dolphin 7.1.14 - DateAndTime implemented
- [] Hashing
    - [] Collections do not implement #hash therefore #testDictionary fails
- [] Character Arrays
    - [] Symbols do not support Unicode codepoints
    - [] Cannot construct a string containing characters in the unicode range (via #with: family of selectors)


## Needed Tests

- [x] Ensure reflected instance variables appear in remoteSelf
- [x] Codify rules for reflected/unreflected instance variables with tests
    - [x] Reflected instance variables appear in remoteSelf
    - [x] Instance variables in concrete classes do not appear
    - [x] Instance variables declared in RsrService and above are not mirrored
- [] RsrSocketStream
    - [] Internal buffer of written data
- [x] RsrSocket
    - [x] Data available?
    - [x] Read available data
- [x] Unicode
    - [x] Strings
    - [x] Symbols
    - [x] Characters
- [] Service
    - [x] Mirrored instance variable reference unsupported object
    - [x] Mirror instance variables containing each supported species
    - [x] Send and return Service
        - [x] Referencing another service
        - [x] Referencing all data objects
- [] Collection
    - [] reference to self in collection

## Needed Functionality

- [] Stream Decorator (implementing Stream interface)
    - [] RsrBufferingSocketStream

## Bugs

- [x] All RsrCommandProcessors should #stop regardless of whether they are waiting on a socket or a queue