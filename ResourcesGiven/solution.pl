:- [dataStructures].
:- [counter].

/*
blind arbitrary cost  search
*/

/*
solution(+Problem, ?SolutionCost, ?Solution)

we assume that the heuristic file is loaded with the heuristic being
the h/3 predicate, h(State, Actions, HValue), where in the case of
TSP, Actions  is the road network 

we assume Problem is fully instantiated and depends on the domain file, 
tsp.pl which will translate the native problem into the standard InitState, GoalState pair
*/

solution(NativeProblem, SolutionCost, SolutionPath) :-
    empty_OpenList(EmptyOpenList),
    make_problem(NativeProblem, problem(InitState, GoalState), Actions), %% format of NativeProblem is domain dependent 
    retractall(goalState(_)),  %% get rid of old goal states
    asserta(goalState(GoalState)), %% get new goal state for use by heuristic  
    make_openNode([state(InitState), gValue(0), parent(nil)], InitNode),
    openNode_gValue(InitNode, Priority),
    add_OpenList(EmptyOpenList, Priority, InitNode, OpenList),
    empty_ClosedList(EmptyClosedList),
    removeCounter(expanded),
    initialiseCounter(expanded),
    solution(Actions, GoalState, OpenList, EmptyClosedList, SolutionCost, SolutionPath),
    counter(expanded, Expanded),
    write('Nodes expanded = '),
    writeln(Expanded).

/*
solution(+Actions, +GoalState, +OpenList, +ClosedList, -SolutionCost, -SolutionPath)
*/
/* base case */
solution(_, GoalState, OpenList, ClosedList, SolutionCost, SolutionPath) :-
    min_OpenList(OpenList, SolutionCost, OpenNodeReached),
    openNode_state(OpenNodeReached, GoalState), %% this assumes that the goal test is equality with a goal state
    extractSolution_ClosedList(ClosedList, OpenNodeReached, SolutionPath).

/* recursive case 
remove best node, ParentOpenNode, from open list
ParentState <- ParentOpenNode.state
if ParentState in closed list
then new closed <- old closed
     new open <- updated open
else new closed <- old closed + parentnode
     children <- expand(parentnode)
     new open list <- updated open list + children
*/

solution(Actions, GoalState, OpenList, ClosedList, SolutionCost, SolutionPath) :-
   get_OpenList(OpenList, _, ParentOpenNode, ParentRemovedOpenList),
   openNode_state(ParentOpenNode, ParentState),
   (in_ClosedList(ClosedList, ParentState, ClosedNode) ->
      (NewClosedList = ClosedList,
       NewOpenList = ParentRemovedOpenList)
   ;  (openNode_to_ClosedNode(ParentOpenNode, ClosedNode),
       add_ClosedList(ClosedList, ParentState, ClosedNode, NewClosedList),
       expand_OpenNode(ParentOpenNode, Actions, ChildrenNodes),
       addList_OpenList(ChildrenNodes, ParentRemovedOpenList, NewOpenList))),
   solution(Actions, GoalState, NewOpenList, NewClosedList, SolutionCost, SolutionPath).

