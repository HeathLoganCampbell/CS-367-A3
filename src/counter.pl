%% -*-prolog-*-
/*
counter predicates
====================

examples of use:
- To create a counter (with an initial value of 0)
initialiseCounter(openList)

- To increment (add 1 to) a counter
    (if counter doesn't exist then create it)
incrementCounter(openList)

- To decrement (subtract 1 from) a counter
    (if count becomes 0 then counter is removed)
decrementCounter(openList)

- To access the current value of a counter
counter(openList, Value)

*/

removeCounter(Counter) :-
	retractall(counter(Counter, _)).

initialiseCounter(Counter) :-
	removeCounter(Counter),
	assert(counter(Counter, 0)).

addToCounter(Counter, Amount) :-
	retract(counter(Counter, N)),
	NewN is N + Amount,
	assert(counter(Counter, NewN)).

incrementCounter(Counter) :-
	(retract(counter(Counter, N))
	->
	    NewCount is N + 1,
	    assert(counter(Counter, NewCount))
	;
	    assert(counter(Counter, 1))).

decrementCounter(Counter) :-
	retract(counter(Counter, N)),
	NewCount is N - 1,
	(NewCount > 0
	->
	    assert(counter(Counter, NewCount))
	;
	    true).
