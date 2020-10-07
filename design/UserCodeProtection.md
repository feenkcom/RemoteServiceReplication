# Debugging

## Purpose

To describe the handling of user code in RSR.

## User Code

User code refers to user defined code activated by RSR. This happens when a message is sent to #remoteSelf in a Service. The message sent to #remoteSelf defines a calling context. #remoteSelf is a proxy that will automatically forward a message to the Service's remote peer. The remote evaluation defines the evaluation context.

User code is evaluated in a concurrent (forked) context. Applications may be implemented concurrently and the framework should reflect this property.

User code handling must exhibit the follow properties.

- A response must be sent to the calling context
- Unhandled errors that are not handled by a Service should be signaled in the calling context
- Badly behaved user code should not break RSR
- Consistent framework behavior across supported Smalltalk environments

## High Level Plan for User Code Evaluation

The result of evaluating this pseudocode should be provided to the client. If the result is a RemoteError, it should be signal in the calling context. Otherwise, the result should be returned as the result of the send to #remoteSelf in the calling context.

```smalltalk
send: aMessage
to: aService

    self
        protectedEvaluation: [^aMessage sendTo: aService]
        onUnhandledException:
            [:unhandledException | | handleResult |
            handleResult := self
                protectedEvaluation:
                    [aService
                        debugMessage: aMessage
                        signaling: unhandledException]
                onUnhandledException:
                    [:debugException |
                    ^RemoteError reportingOn: debugException].
            ^unhandledException isResumable
                ifTrue: [unhandledException resume: handleResult]
                ifFalse: [RemoteError reportingOn: unhandledException]]
```

The result of #send:to: will function of the response for the message send. A normal object will be returned as a result in the calling context. A RemoteError will be signaled in the calling context.

#protectedEvaluation:onUnhandledException: in this pseudocode will never return normally. The block arguments are expected to handle evaluation and returning of a result. In this case, the block arguments use explicit returns.

Each platform is free to implement the activation and handling of user code, as long as it behaves correctly.

RSR defines an exception as unhandled if and only if an an #on:do: and #defaultAction have not resolved the exception.

### GemStone - Protected Evaluation

GemStone will intercept an unhandled exception through the use of #debugActionBlock on each forked Process. This block is activated by the default implementation of #defaultAction. Exceptions implementing #defaultAction are expected to either resolve the exceptional state or defer to #debugActionBlock if they cannot resolve the exceptional state.

GemStone's semantics allow a simple implementation of #protectedEvaluation:onUnhandledException:.

```smalltalk
protectedEvaluation: aProtectedBlock
onUnhandledException: aHandlerBlock

    | process cachedDebugActionBlock |
    process := Process activeProcess.
    cachedDebugActionBlock := process debugActionBlock.
    ^[process debugActionBlock: aHandlerBlock.
    aProtectedBlock value]
        ensure: [process debugActionBlock: cachedDebugActionBlock]
```

## Restrictions

- User code must not set a #debugActionBlock in any Process created and owned by RSR. Doing so may result in unexpected behavior.
