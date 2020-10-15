# Message Send Semantics

Message sends to `#remoteSelf` are asynchronous. The current thread will not wait for a response. Instead, the framework quickly answers a `Promise` object in lieu of the result. RSR guarantees that the `Promise` will be resolved at a future time.

```smalltalk
  | promise |
  promise := remoteSelf calculateResult.

  "The thread will continue execution having obtained a Promise.
  The #calculateResult method will be evaluated and eventually 
  the `Promise` will be resolved with the result."
```

## Promise

A `Promise` represents a placeholder value. `Promise`'s are later resolved -- either fulfilled with a result or broken if a result cannot be obtained.

### `Promise` Interface

- Resolution  _**Do we really want to allow the resolution messages to be sent to the promise itself, rather than the resolver?**_
  - `#fulfill:`
  - `#break:`
- Observation
  - `#when:catch:`
  - `#wait`

_**Do we want a message that retrieves the value from a previously-fulfilled promise? Besides #wait, which probably also does that.**_

### `Promise` Resolution

A `Promise` can be resolved either through fulfillment or through breaking. Fulfillment is the resolution of a `Promise` to a value. This value is provided as the argument to the `#fulfill:` method. Breaking is the notification that a `Promise` will never resolve to a value. A reason is provided to the `#break:` method.

### `Promise` Observation

`Promise`s offer a synchronous and an asynchronous way of observing their resolution. A `Promise` may have multiple observers using a mix of synchronous and asynchronous observation mechanisms.

**Ooh, multiple observers. I hadn't thought that far, but yes, that seems good.**

#### Synchronous Observation

Sending the message `#wait` to a `Promise` will cause the current thread to suspend until it is resolved. After resolution, the thread will resume. If the `Promise` was fulfilled, the value of the `Promise` will be returned from `#wait`. If the `Promise` was broken, a `BrokenPromise` error will be signaled. Sending `#reason` to the `BrokenPromise` exception will return the reason the `Promise` was broken.

_**Is BrokenPromise a subclass of Error? I suppose it should be. Normally you want to handle it, and if you don't you should expect normal unhandled Error response.**_

#### Asynchronous Observation

The `#when:catch:` method is used to register for an asynchronous callback when the `Promise` is resolved.

```smalltalk
aPromise
  when: [:value | "Process the value of the promise"]
  catch: [:reason | "Process the breaking of the promise"]
```

The first argument, named `fulfilledBlock`, is evaluated when the `Promise` has been fulfilled. The block is passed the value of the `Promise` provided to `#fulfill:` during resolution.

The second argument, named `brokenBlock`, is evaluated when the `Promise` has been broken. The block is passed the reason provided to `#break:` during resolution.

As a result of resolution, only one block will be evaluated and the evaluation will occur in a newly forked thread.

## RSR `Promise` Notes

- If a `Promise` is not resolved when the `Connection` is closed, the framework will `#break:` the `Promise` with a reason of `'Connection closed'`.

## Rsr Promise Usage Examples

### Synchronous Example

```smalltalk
  | result |
  result := (remoteSelf evaluate: '3 + 4') wait.
  "result is assigned the value 7 instead of a Promise instance"
```

### Asynchronous Example

```smalltalk
  | promise |
  promise := remoteSelf evaluate: '3 + 4'.
  promise
    when: [:result | Transcript show: '3 + 4 == ' + result printString; cr]
    catch: [:reason | Transcript show: 'Unable to evaluate ''3 + 4'' because ' + reason]
```
