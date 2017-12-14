fact(0, 1).
fact(N, R) :- N > 0, N2 is N-1, fact(N2, R2), R is N*R2.

parent(a, b).
parent(a, c).
parent(b, d).
parent(e, f).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Y, Z).

% get list of ancestors of X
ancestor(X, L) :- bagof(N, ancestor(X, N), L).
has_more_than_one_ancestor(X) :- ancestor(X, [_,_|_]).

