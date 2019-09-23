/* this is the domain file for travelling salesman problems
we will define the following functions here
making states
making  init state and goal state
generating successor
testing for solutions
*/
/* state  structure:
states contain 2 bits of information
- what state is currently being visited, this info is needed for computing successors
- which states have not already been visited, this information is needed for avoiding loops

*/
:- use_module(library(ordsets)).
:- use_module(library(record)).

:- record state(visitingCity, stillToVisitCitySet).
%% visitingCity is name of current city
%% stillToVisitCitySet is set of cities not already visited (not incl visiting city)

/*
make_initState(+StartCity, +RoadNetwork, -InitState)

creates a TSP state with StartCity and StillToVisitCitySet
*/
make_initState(StartCity, RoadNetwork, InitState) :-
    setOfCities(RoadNetwork, NonVisitedCitySet),
    make_state([visitingCity(StartCity), stillToVisitCitySet(NonVisitedCitySet)], InitState).

/*
make_goalState(+StartCity, -GoalState)

creates a TSP state with StartCity and no more cities to visit, 
*/
make_goalState(StartCity, GoalState) :-
    make_state([visitingCity(StartCity), stillToVisitCitySet([])], GoalState).

/*
make_problem/3
make_problem(domainProblem([+StartCity, +RoadNetwork]), -problem(InitState, GoalState), -RoadNetwork)

given the native problem representation translates it into the representation required by 
solution/3 RoadNetwork represents the actions of this domain
*/
make_problem(domainProblem([StartCity, RoadNetwork]), problem(InitState, GoalState), RoadNetwork) :-
    make_initState(StartCity, RoadNetwork, InitState),
    make_goalState(StartCity, GoalState).

/*
successor(+ParentState, +RoadNetwork, (NeighborState, Cost)) :-
    
given ParentState uses RoadNetwork to generate a successor state, NeighborState, for Cost    
*/

successor(ParentState, RoadNetwork, (NeighborState, Cost)) :-
    /*
     a child is legal iff it is a successor 

    */
    state_visitingCity(ParentState, FromCity),
    state_stillToVisitCitySet(ParentState, FromVisitSet),
    FromVisitSet = [_ | _],
    member((FromCity, Neighbors), RoadNetwork),
    member((ToCity, Cost), Neighbors),
    member(ToCity, FromVisitSet),
    ord_del_element(FromVisitSet, ToCity, NewVisitSet),
    make_state([visitingCity(ToCity), stillToVisitCitySet(NewVisitSet)], NeighborState).

successor(ParentState, _, (NewState, 0)) :- 
    /*
    the cost between any city and itself is always 0!!
    no matter what it says in the road network, but you can only revisit 
    yourself once
    */
    state_stillToVisitCitySet(ParentState, FromVisitSet),
    FromVisitSet = [_ | _], %% checking for non-empty stillToVisitCitySet
    state_visitingCity(ParentState, VisitingCity),
    ord_del_element(FromVisitSet, VisitingCity, ToVisitSet),
    make_state([visitingCity(VisitingCity), stillToVisitCitySet(ToVisitSet)], NewState).


setOfCities(RoadNetwork, SetOfCities) :-
    findall(City, member((City, _), RoadNetwork), SetOfCities).
