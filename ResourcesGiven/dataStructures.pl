/*  Search Data Structures */

%% open & closed node structures:
:- use_module(library(record)).

/* 
openNode_to_ClosedNode(+OpenNode, -ClosedNode)

translates an open node into a closed node (i.e., changes the type)
*/
openNode_to_ClosedNode(OpenNode, ClosedNode) :-
    openNode_state(OpenNode, State),
    openNode_gValue(OpenNode, GValue),
    openNode_parent(OpenNode, Parent),
    make_closedNode([state(State), gValue(GValue), parent(Parent)], ClosedNode).

/* 
expand_OpenNode(+OpenNode, +Actions, ?SuccessorNodes)
 
this depends upon "successor/3" being defined for the given domain
 which will generate all the children including ones already expanded

*/

expand_OpenNode(OpenNode, Actions, SuccessorNodes) :-
    openNode_state(OpenNode, ParentState),
    openNode_gValue(OpenNode, ParentGValue),
    findall(SuccessorStateAndCost,
	    successor(ParentState, Actions, SuccessorStateAndCost),
	    SuccessorStateAndCosts),
    findall(ChildNode,
	    (member((ChildState, ChildCost), SuccessorStateAndCosts),
	     ChildGValue is ParentGValue + ChildCost,
	     make_openNode([state(ChildState),
			    gValue(ChildGValue),
			    parent(ParentState)],
			   ChildNode)),
	    SuccessorNodes),
    incrementCounter(expanded).


/* structure of open and closed nodes */
:- record openNode(state, gValue, parent).  
:- record closedNode(state, gValue, parent).


/*
from https://www.swi-prolog.org/pldoc/man?section=record
A record declaration creates a set of predicates through term-expansion. We describe these predicates below. In this description, <constructor> refers to the name of the record (`point' in the example above) and <name> to the name of an argument (field).

default_<constructor>(-Record)
Create a new record where all fields have their default values. This is the same as make_<constructor>([], Record) .

make_<constructor>(+Fields, -Record)
Create a new record where specified fields have the specified values and remaining fields have their default value. Each field is specified as a term <name>(<value>). See example in the introduction.

make_<constructor>(+Fields, -Record, -RestFields)
Same as make_<constructor>/2, but named fields that do not appear in Record are returned in RestFields. This predicate is motivated by option-list processing. See library library(option).

<constructor>_<name>(Record, Value)
Unify Value with argument in Record named <name>.  Note this is not called `get_' as it performs unification and can perfectly well instantiate the argument.

<constructor>_data(?Name, +Record, ?Value)
True when Value is the value for the field named Name in Record. This predicate does not perform type-checking.

set_<name>_of_<constructor>(+Value, +OldRecord, -NewRecord)
Replace the value for <name> in OldRecord by Value and unify the result with NewRecord.
set_<name>_of_<constructor>(+Value, !Record)
Destructively replace the argument <name> in Record by Value based on setarg/3. Use with care.

set_<constructor>_fields(+Fields, +Record0, -Record)
Set multiple fields using the same syntax as make_<constructor>/2, but starting with Record0 rather than the default record.

set_<constructor>_fields(+Fields, +Record0, -Record, -RestFields)
Similar to set_<constructor>_fields/4, but fields not defined by <constructor> are returned in RestFields.

set_<constructor>_field(+Field, +Record0, -Record)
Set a single field specified as a term <name>(<value>).

*/
/*

OpenList is a priority queue, and closed list is a hashtable.
Unfortunately for closed lists, we cannot use dicts as hashtables, 
so we will need to use the prolog association lists (AVL trees)
For open lists, we can just use heaps (i.e., priority queues)
*/

/*OpenList predicates
=======================*/

/* empty_OpenList(?OpenList) */
empty_OpenList(OpenList) :- empty_heap(OpenList).


/* add_OpenList(+OldOpenList, +Priority, +OpenNode, -NewOpenList) 

adding OpenNode with Priority to OldOpenList to get NewOpenList
*/
add_OpenList(OldOpenList, Priority, OpenNode, NewOpenList) :-
   add_to_heap(OldOpenList, Priority, OpenNode, NewOpenList).

/*
addList_OpenList(+ListNewOpenNodes, +OldOpenList, -NewOpenList)

ListNewOpenNodes is a list of FValue-OpenNode pairs, where FValue is the priority
value for OpenList

*/
addList_OpenList(ListNewOpenNodes, OldOpenList, NewOpenList) :-
   findall(GValue-OpenNode, 
	   (member(OpenNode, ListNewOpenNodes),
	    openNode_gValue(OpenNode, GValue)),
	   NewOpenListNodes),
   list_to_heap(NewOpenListNodes, NewHeap),
   merge_heaps(NewHeap, OldOpenList, NewOpenList).

   

/*
get_OpenList(+OldOpenList, -Priority, -OpenNode, -NewOpenList)

removes the least Priority (i.e., the best) OpenNode from OldOpenList giving NewOpenList
*/
get_OpenList(OldOpenList, Priority, OpenNode, NewOpenList) :-
   get_from_heap(OldOpenList, Priority, OpenNode, NewOpenList).

/* 
min_OpenList(+OpenList, -Priority, -OpenNode)

returns the least Priority OpenNode from OpenList without "removing" it
 
*/
min_OpenList(OpenList, Priority, OpenNode) :-
   min_of_heap(OpenList, Priority, OpenNode).

/*ClosedList predicates
=========================*/
/*
empty_ClosedList(?ClosedList)

create/checks if ClosedList is an empty ClosedList
*/
empty_ClosedList(ClosedList) :- empty_assoc(ClosedList).

/*
add_ClosedList(+OldClosedList, +State, +ClosedNode, -NewClosedList)

adds CLosedNode with key State to OldClosedList giving NewClosedList
*/
add_ClosedList(OldClosedList, State, ClosedNode, NewClosedList) :-
   put_assoc(State, OldClosedList, ClosedNode, NewClosedList).

/*
in_ClosedList(+ClosedList, +State, -ClosedNode)

checks whether the key State is in ClosedList and if so returns its associated node
*/
in_ClosedList(ClosedList, State, ClosedNode) :-
   get_assoc(State, ClosedList, ClosedNode).


/*
extractSolution_ClosedList(+ClosedList, +OpenNodeReached, -Solution)

uses the state and the parent in OpenNodeReached to build up the solution path
*/
extractSolution_ClosedList(ClosedList, OpenNodeReached, Solution) :-
    openNode_to_ClosedNode(OpenNodeReached, FauxClosedNodeReached),
    extractPath([], FauxClosedNodeReached, ClosedList, Solution).

extractPath(AccumulatingPath, CurrentStepClosedNode, ClosedList, ReturningPath) :-
    closedNode_state(CurrentStepClosedNode, ClosedState),
    state_visitingCity(ClosedState, City),
    closedNode_parent(CurrentStepClosedNode, ParentClosedState),
    (ParentClosedState == nil ->
	ReturningPath = [City | AccumulatingPath]
    ;   in_ClosedList(ClosedList, ParentClosedState, ParentClosedNode),
	extractPath([City | AccumulatingPath], ParentClosedNode, ClosedList, ReturningPath)).



    




