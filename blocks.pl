% A list of all blocks in the world.
% We can modify this if we want more/less blocks.
blocks([a, b, c, d]).

% A term X is a block if it is a member of the list returned by blocks/1.
block(X) :-
    blocks(BLOCKS),
    member(X, BLOCKS).

% Utility predicates:

% notequal(X1, X2) succeeds when X1 and X2 are different.
notequal(X, X) :- !, fail.
notequal(_, _).

% substitute(E, E1, OLD_L, NEW_L)
% NEW_L is OLD_L but with the first occurence of element E replaced by E1.
% Assumed no duplicates.
substitute(X, Y, [X|T], [Y|T]). % head matches X? replace it.
substitute(X, Y, [H|T], [H|T1]) :- % head does not match? recurse on tail
    substitute(X, Y, T, T1).

% remove(E, List, Result)
% Result is List with the first occurrence of E removed.
remove(X, [X|T], T).
remove(X, [H|T], [H|T1]) :-
    remove(X, T, T1).

% add(E, List, [E|List])
% Just adds an element at the front.
add(X, L, [X|L]).

% Move operators
% move(X, Y, Z, S1, S2) holds when state S2 is obtained from S1 by 
% moving block X from "support" Y onto "supprt" Z.

% Rule 2: Move a clear block X from block Y onto another clear block z.
% Effects: [on, X, Y] replaced by [on, X, Z] (X on Y, X on Z)
%          [clear, Z] replaced by [clear, Y] (Z is no longer clear, Y is)
move(X, Y, Z, S1, S2) :-
    member([clear, X], S1), % X is clear here
    member([on, X, Y], S1), block(Y),
    member([clear, Z], S1), notequal(X, Z),
    substitute([on, X, Y], [on, X, Z], S1, INT),
    substitute([clear, Z], [clear, Y], INT, S2).

% Rule 3: Move a clear block X from a block Y onto the table
% Effects: [on, X, Y] replaced by [on, X, table]
%          Y becomes clear: add [clear, Y]
move(X, Y, table, S1, S2) :-
    member([clear, X], S1), % X is clear
    member([on, X, Y], S1), block(Y),
    substitute([on, X, Y], [on, X, table], S1, INT1),
    add([clear, Y], INT1, S2).

% Rule 4: Move a clear block X from the table onto a clear block Y.
% Effects: [on, X, table] replaced by [on, X, Y]
%          Y is no longer clear, so remove [clear, Y]
move(X, table, Y, S1, S2) :-
    member([clear, X], S1), % X is clear
    member([on, X, table], S1), % X currently on the table
    member([clear, Y], S1), block(Y), notequal(X, Y),
    substitute([on, X, table], [on, X, Y], S1, INT1),
    remove([clear, Y], INT1, S2).

% There is a path from state S1 to state S2 when there is a move from S1 to S2.
path(S1, S2):-
	move(X, Y, Z, S1, S2).

% Connect is the symmetric version of path: states S1 and S2 are connected if 
% there is a path from S1 to S2 or a path from S2 to S1.
connect(S1, S2) :- path(S1, S2).
connect(S1, S2) :- path(S2, S1).

% Test state, can remove when you do your own thing:
test_state1([[on, a, b],
             [on, b, table],
             [on, c, table],
             [clear, a],
             [clear, c]]).