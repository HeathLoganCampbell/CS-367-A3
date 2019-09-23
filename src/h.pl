%% h/3 the zero heuristic

% h(_, _, 0).

/*
h(+State, +Action, ?HValue)
*/
% h(State, Action, HValue) :-
%   HValue is 0.
  
% openNode(state(a,[a,b,c,d,e,f]),0,nil)
% openNode(state(%Current Node%, [%unvisisted nodes%]), %currentCode%, %LastState%)
% openNode(state(c,[e]), 25, state(d,[c,e]))
% State = openNode(state(a,[a,b,c,d,e,f]), 0, nil)
% Action = [(a,[(b,5),(f,6)]),(b,[(a,5),(e,1),(c,4)]),(c,[(b,4),(f,9),(d,2)]),(d,[(e,3),(c,2),(f,7)]),(e,[(b,1),(d,2)]),(f,[(a,6),(c,9),(d,7)])]
% h(State, Action, HValue).



/**
 SUMS UP THE TOTAL AKA ITS THE h() function
 findall(CheapestNode, ([a,c,d,e,f] = Unseen, six_cities(RoadNetwork), member(Parent,Unseen) ,findSmallestInEdge(RoadNetwork, Parent, EdgeList),  smallestWeight(EdgeList, CheapestNode)), CheapestPaths), derpSumShitFuckAids(CheapestPaths, Sum).

 Lists all the cheapest in degree nodess of unseen nodes
 [a,c,d,e,f] = Unseen, six_cities(RoadNetwork), member(Parent,Unseen) ,findSmallestInEdge(RoadNetwork, Parent, EdgeList),  smallestWeight(EdgeList, CheapestNide).

 [a,c,d,e,f] = Unseen, six_cities(RoadNetwork), member(Parent,Unseen) ,findSmallestInEdge(RoadNetwork, Parent, EdgeList),  smallestWeight(EdgeList, CheapestNode).
  findall(CheapestNode, ([a,c,d,e,f] = Unseen, six_cities(RoadNetwork), member(Parent,Unseen) ,findSmallestInEdge(RoadNetwork, Parent, EdgeList),  smallestWeight(EdgeList, CheapestNode)), CheapestPaths).




 six_cities(RoadNetwork),
 findSmallestInEdge(RoadNetwork, a, EdgeList), 
 smallestWeight(EdgeList, SmallestEdge).

 Where B is the node we are investiating,
 This will return the smallest edge
 **/
six_cities(RoadNetwork) :-
  [
    (a, [(b, 5), (f, 6)]),
    (b, [(a, 5), (e, 1), (c, 4)]),
    (c, [(b, 4), (f, 9), (d, 2)]),
    (d, [(e, 3), (c, 2), (f, 7)]),
    (e, [(b, 1), (d, 2)]),
    (f, [(a, 6), (c, 6), (d, 7)])
  ] = RoadNetwork.

single_cities(SingleState) :-
  (c, [(b, 4), (f, 9), (d, 2)]) = SingleState.


% find all b's ins
% find if there is b in node edges


findIfPresent((H, T), Parent, Cost) :-
  writeln('Fuck'),
  writeln(Parent),
  writeln(H),
  writeln(T),
  member((Parent, Cost), T).
  %  member(X, RoadNetwork).


% single_cities(SingleState), findIfPresent(SingleState, b).
% findIfPresent(SingleState, b).

findEdges(Road, Parent, Edge) :-
   member((EdgeNode, List) , Road),
   member((Parent, Cost), List),
   edge(EdgeNode, Cost) = Edge.
  
/*
These function will return the smallest of the two
minWeight(edge(a, 1), edge(b, 3), Min)
*/
minWeight(EdgeOne, EdgeTwo, EdgeTwo) :- 
  edge(_, Cost) = EdgeOne,
  edge(_, CurCheapestCost) = EdgeTwo,
  Cost >= CurCheapestCost.

minWeight(EdgeOne, EdgeTwo, EdgeOne) :- 
  edge(_, Cost) = EdgeOne,
  edge(_, CurCheapestCost) = EdgeTwo,
  Cost < CurCheapestCost.

/*
this function will return the smallest edge in a list

Example:
  smallestWeight([edge(a, 1), edge(b, 2), edge(c, 3)], MinEdge).

*/
smallestWeight([LastItem], LastItem).

smallestWeight([ H | T ], CheapestEdge) :-
  smallestWeight(T, SubCheapestEdge),
  minWeight(H, SubCheapestEdge, CheapestEdge).


findSmallestInEdge(Road, Parent, EdgesList) :- 
  findall(X, (findEdges(Road, Parent, X)), EdgesList).

/**
We now want to get the smallest edges for all unvisited nodes

Step 1, 
  loop through all unvisited nodes
Step 2, 
  Total up their values

b,[a,c,d,e,f]
g = 5, 
h = 16, 
f = 21

h( openNode(state(b,[a,c,d,e,f]), 0, 0), [(a,[(b,5),(f,6)]),(b,[(a,5),(e,1),(c,4)]),(c,[(b,4),(f,9),(d,2)]),(d,[(e,3),(c,2),(f,7)]),(e,[(b,1),(d,2)]),(f,[(a,6),(c,9),(d,7)])], HValue).

[a,c,d,e,f]
find smallest in degree for a



**/

/**
six_cities(RoadNetwork), derpSumShitFuckAids(RoadNetwork, [a,c,d,e,f], Sum).
**/
derpSumShitFuckAids([ edge(Node, Cost) ], Cost).


derpSumShitFuckAids( [ H | T ], Sum) :-
  derpSumShitFuckAids( T, SubSum),
  edge(Node, Cost) = H,
  Sum is Cost + SubSum.


h(state(_, []), _, 0).

% ExampleState = openNode(state(b,[a,c,d,e,f]), 0, 0).
% ExampleAction = [(a,[(b,5),(f,6)]),(b,[(a,5),(e,1),(c,4)]),(c,[(b,4),(f,9),(d,2)]),(d,[(e,3),(c,2),(f,7)]),(e,[(b,1),(d,2)]),(f,[(a,6),(c,9),(d,7)])].
h(State, Action, HValue) :-
  % writeln(State),
  % writeln(Action),
  state(CurrentNode, UnvisitedNodes) = State,
  findall(CheapestNode, (member(Parent, UnvisitedNodes), findSmallestInEdge(Action, Parent, EdgeList),  smallestWeight(EdgeList, CheapestNode)), CheapestPaths), derpSumShitFuckAids(CheapestPaths, HValue).
  % go through the list add them up

  
  % % member((Node, RoadOptions), Action),
  % findSmallestInEdge(Action, a, EdgeList),
  % smallestWeight(EdgeList, HValue).

  
  
  % \+ (edge(_, OtherCost),
  %     Cost < OtherCost).



 % six_cities(RoadNetwork), findEdges(RoadNetwork, b, Edge)
 % (c, 6)
% six_cities(RoadNetwork),findSmallestInEdge(RoadNetwork, b, Edge).
