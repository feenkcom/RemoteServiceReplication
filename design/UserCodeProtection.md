# Debugging

## Purpose

To describe the handling of user code in RSR.

## User Code

User code refers to any code written by the user of RSR. All methods in Services, excluding methods starting with $_ and sent by the framework, are considered user code.

### User Code Evaluation

User code is evaluated in a concurrent (forked) context. Applications may be implemented concurrently and the framework should reflect this property.

User code handling must exhibit the follow properties.

- The `Promise` associated with a send must be resolved
- Errors not handled by the Service should break the `Promise`
- Badly behaved user code should not break RSR
- Consistent framework behavior across supported Smalltalk environments

## Resolver

Each message send is associated with a `Resolver`. The `Resolver` provides an interface similar to the resolution interface of the remote `Promise`. This interface is used resolve the remote `Promise`. 

### Resolver Interface

- Resolution Methods
    - `#fulfill:`
    - `#break:`
- Testing Methods
    - `#hasResolved`

The argument to either resolution method must be an object supported by RSR -- Services or data objects. In the event that it is not, the promise will be broken. Information about the unsupported object will be provided as the reason the `Promise` was broken.

Only the first call to a resolution method is meaningful. Further calls will result in an error.

## High Level Plan for User Code Evaluation

This pseudocode represents the basic semantics of handling the activation of user code. The behavior should be implemented on each supported platform and should conform to the behavior of GemStone's implementation.

```smalltalk
send: aMessage
to: aService
using: aResolver

    [[^aResolver fulfill: (aMessage sendTo: aService)]
        on: UnhandledException
        do:
            [:wrapper |
            [ | exception debugResult |
            exception := wrapper exception.
            debugResult := aService
                debugMessage: aMessage
                raising: exception
                using: aResolver.
            ^aResolver hasResolved
                ifTrue:
                    ["If the debugger resolved the Promise, we consider the message
                    as having been fully processes and end the current process."
                    self]
                ifFalse:
                    [exception isResumable
                        ifTrue:
                            ["Match the behavior of #defaultAction. If the exception is resumable,
                            the result of #signal should be whatever was returned by the debug call."
                            wrapper resume: debugResult]
                        ifFalse:
                            ["If the exception is not resumable and the debugger did not cut the stack,
                            we do all we can -- break the Promise w/ as much debug information as possible."
                            aResolver break: (Reason forException: exception)]]]
                on: UnhandledException
                do:
                    [:debugExceptionWrapper |
                    ^aResolver break: (Reason forException: debugExceptionWrapper exception)]]]
        ensure:
            [aResolver hasResolved
                ifFalse: [aResolver break: 'Message send terminated without a result']]
```

By default, `#debugMessage:raising:using:` would have an implementation similar to this. This method is treated just like #defaultAction. If it returns a result and the exception is resumable, we resume with the result of its evaluation. If not, we break the promise and provide debug information as the reason.

```smalltalk
debugMessage: aMessage
raising: anException
using: aResolver

    aResolver break: (Reason forException: anException)
```

In both of these examples, `Reason` is a stand-in for an Object which converts an Exception into a String containing sufficient debug information. For instance, the string may contain the class of the original exception, its messageText, and the stack as captured at time of reporting.

### Service Debugging Hook

Should an unhandled exception occur during the evaluation of a `Message`, RSR will call into the receiving `Service` giving it an opportunity to debug the exception. This is accomplished through the use of the `#debugMessage:raising:using:` selector. RSR will provide a default implementation which breaks the promise with information about the exception and stack.

This method could also fulfill the `Promise` using the provided `Resolver`.

## GemStone Specific Hooks

GemStone will hook into the `#debugActionBlock` hook associated with the `Process`. The block will signal an UnhandledException containing the original exception.

This will traverse the stack and eventually find the `#on:do:` handler created in the `#send:to:using:` method which will handle the exception.

```smalltalk
[:exception | UnhandledException signalWith: exception]
```

## Restrictions

- User code must not set a #debugActionBlock in any Process created and owned by RSR. Doing so may result in unexpected behavior.
