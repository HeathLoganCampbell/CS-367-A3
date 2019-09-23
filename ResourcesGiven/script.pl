/* 
This file  is used to load all the files needed to load the files
necessary to run the blind search, you can modify it to add whatever
testfiles you want loaded.

*/
:- [solution, tsp, h].

nativeProblem(domainProblem([a,
			     [(a, [(b, 5), (f, 6)]),
			      (b, [(a, 5), (e, 1), (c, 4)]),
			      (c, [(b, 4), (f, 9), (d, 2)]),
			      (d, [(e, 3), (c, 2), (f, 7)]),
			      (e, [(b, 1), (d, 2)]),
			      (f, [(a, 6), (c, 9), (d, 7)])]])).

example(Cost, Path) :-
    nativeProblem(NativeProblem),
    solution(NativeProblem, Cost, Path).
