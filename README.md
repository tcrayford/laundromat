# laundromat
(air your dirty state machines)

A Clojure library for generative testing of stateful programs (via test.check), via a model of state transitions.

## NOT READY
## THIS IS A SKETCH

This is not a release (for one thing the concurrency stuff is like half
implemented, and the non concurrent checker code is *horrible*. This is purely
a sketch of an api for feedback purposes.

The concurrent checker has been written/rewritten a couple of times - that
thing is pretty hard to implement well (especially in a completely
non-disciplined manner like I was initially trying). It likely won't be ready
for a while longer.

## GOALS

Laundromat at this state only concerns itself with two kinds of state machine checking:

- single threaded checking of a sequence of states
- concurrent atomicity checking

It is unlikely to ever concern itself with checking other models of
concurrency, or models where actions can fail, or models where timing out of
state machine actions isn't a failure.

This was heavily inspired by John Hughes' talk at Clojure West 2014:

https://www.youtube.com/watch?v=zi0rHwfiX1Q

And one of the QC state machine papers:

http://publications.lib.chalmers.se/records/fulltext/125252/local_125252.pdf

## API

I've diverged significantly from the original erlang quickcheck. State machines
are just maps, with each key representing a potential transition:

```clojure
(def ticker-machine
  {:initial-state  {:model/initial   (constantly 0)
                    :actual/initial  (fn [] (atom 0))}

   :take-ticket  {:model/next     (fn  [previous-state args]
                                    (+ previous-state (first args)))
                  :args           (gen/tuple (gen/return 1))
                  :actual/command (fn [ticker] (swap! ticker inc))
                  :postcondition  (fn [actual-result actual-state model-state args]
                                    (assert (= actual-result model-state) (str "expected ticket " actual-result " to equal model " model-state)))}

   :reset        {:precondition   (fn [model-state args] (not (= model-state 0)))
                  :model/next     (constantly 0)
                  :actual/command (fn [ticker] (reset! ticker 0))}})
```

`:initial-key` represents the initial state of the system, before any commands are

Each transition itself is created by a map with various elements. Here's a
brief walk through each key:

- `:model/next`     : a function that takes the model state, and the arguments, and advances the model state according to the transition
- `:args`           : a test.check generator that should return a sequence of arguments. Likely you'll use `gen/tuple` here.
- `:actual/command` : the function that tells the system under test to undergo a transition. Is called with `(apply command args)`
- `:precondition`   : a function to check if this state transition is valid, based *only* on the existing model state and the args
- `:postcondition`  : a function to check if the result of calling the real system, and the model, with a certain set of arguments
                      results in a correct transition. This should throw an exception to represent failure.

Later on, I'll likely add a `:weight` key that lets you weight how often
commands are generated, but that's not really useful here.

After you've written a state machine specification, you can test it using `test.check` and a function from laundromat:

```clojure
(defpec state-machine-test 100
  (run-state-machine ticker-machine))
```

Here's roughly what happens when you call that function:

- a `test.check` generator turns the model into a sequence of commands (with arguments), using `gen/such-that` to ensure that preconditions are all satisfied
- the runner initializes the state of the system under test
- the runner loops over commands. For each one, it applies it to the real system, then the model, then checks the postcondition (if there is one)

If there's a failure, the usual `test.check` shrinking mechanisms kick in to shrink the generated command sequence.

I've spent the past couple of weeks using this api to test a bunch of stateful stuff, and it's held up well so far.

However (and this is where you come in!): I'd *love* feedback on this api design. Specific things I'd love to hear about:

- (most importantly) Are there flaws in the model that means it can't test a system that you want to test?
  (barring the initial constraints set out in GOALS)
- does the transition centric model make sense? I think it plays pretty well
  with the epochal time model that clojure embraces - the model is just a
  sequence of pure function calls.
- are the names for the keys in each transition good? Can you think of better
  ones? I'm using namespaced keys here, because coming up with names that make
  sense, and are different between the model and the SUT transition functions is
  difficult

## Remaining implementation work

- Concurrent checker needs a *tonne* of work
- Error messages are thrown around weirdly, so stack traces are pretty fucked
- laundromat.core is just a sketch, and should be rewritten

## License

Copyright © 2014 Tom Crayford

Distributed under the Eclipse Public License version 1.0
