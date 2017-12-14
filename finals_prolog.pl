% fall 2013 finals
link(san_diego, seattle).
link(seattle, dallas).
link(dallas, new_york).
link(new_york, chicago).
link(new_york, seattle).
link(chicago, boston).
link(boston, san_diego).

path_2(A, B) :- link(A, C), link(C, B).

path_3(A,B) :- path_2(A, C), link(C, B).

% case for N = 1
path_N(A,B,N) :- N = 1, link(A, B).

% case for N > 1
path_N(A,B,N) :- N > 1, link(A, C), NT is N-1, path_N(C, B, NT).


path(A, B) :- path_helper(A, B, [A]).
% In path_helper below, Seen is the cities we have see so far, so we
% can avoid cycles.
path_helper(A, B, Seen) :- link(A,B), not(member(B, Seen)).
path_helper(A, B, Seen) :- link(A,C), not(member(C, Seen)) , path_helper(C, B, [C|Seen]).


zip([],[],[]).
zip([H1|T1],[H2|T2],[[H1,H2]|T3]) :- zip(T1, T2, T3).

% part(L,P,R1,R2) which holds if R1 contains all the elements of L which are smaller or equal
% to P, and R2 contains all the elements of L which are greater than P

part( [], _, [], []).
part( [H|T], P, [H|T1], R2) :- P >= H, part(T, P, T1, R2).
part( [H|T], P, R1, [H|T2]) :- P < H, part(T, P, R1, T2).



%qsort(_, []) :- false.
%qsort([], _) :- false.

qsort([],[]).
qsort([H|T],R) :- part(T, H, R1, R2), qsort(R1, RR1), qsort(R2, RR2), append(RR1, [H|RR2], R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% winter 2013 finals

remove_all( _ , [] , [] ).
remove_all( X , [H1|T1] , [H2|T2] ) :- X = H1, remove_all(X, T1, [H2|T2]).
remove_all( X , [H1|T1] , [H2|T2] ) :- not(X = H1), H1 = H2, remove_all( X, T1, T2).

remove_first( _ , [] , [] ).
remove_first( X , [X|T] , R ) :- T = R.
remove_first( X , [H1|T1] , [H2|T2] ) :- not(X = H1), H1 = H2, remove_first(X, T1, T2).


prefix( [] , _ ).
prefix( [H1|T1] , [H2|T2] ) :- H1 = H2, prefix(T1, T2).

segment( [H1|T1] , [H2|T2] ) :- H1 = H2, prefix(T1, T2).
segment( [H1|T1] , [_|T2] ) :- segment([H1|T1], T2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% winter 2012 finals

sorted([]).
sorted([_]).
sorted([A,B|T]) :- A =< B, sorted([B|T]).

sort1(L1,L2) :- permutation(L1, X), sorted(X), L2 = X, !.


split([], [], []).
split([X], [X], []).
split([X | T], [X|T1], L3) :- split(T, L3, T1).

merge([],L,L).
merge(L,[],L).
merge( [H1|T1], [H2|T2], [H1|T3]) :- H1 < H2, merge(T1, [H2|T2], T3).
merge( [H1|T1], [H2|T2], [H2|T3]) :- H2 < H1, merge([H1|T1], T2, T3).


merge_sort([], []).
merge_sort([X], [X]).
merge_sort(L,S) :- split(L, L1, L2), sort1(L1, R1), sort1(L2, R2), merge(R1, R2, S). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% winter 2011 finals

sat(var(X)) :- X = 1.
sat(not(var(X))) :- X = 0.
sat(and([])).

%% Fill in the other case(s) for ‘‘and’’ here:
sat(and([X | Tail])) :- sat(X), sat(and(Tail)).
sat(or([])) :- fail.

%% Fill in the other case(s) for ‘‘or’’ here:
sat(or([X | Tail])) :- sat(X).
sat(or([_ | Tail])) :- sat(or(Tail)).


bool(X) :- X = 0.
bool(X) :- X = 1.
bools([]).
bools([X | Tail]) :- bool(X), bools(Tail).


allsat(L, F) :- bools(L), sat(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fall 2005 finals

actor(xmen,jackman).
actor(xmen,berry).
actor(scoop,jackman).
actor(scoop,johanssen).
actor(lost_in_translation,murray).
actor(lost_in_translation,johanssen).
actor(ghostbusters,murray).
actor(ghostbusters,akroyd).
actor(batmanreturns,bale).
actor(batmanreturns,caine).
actor(dirtyrottenscoundrels,martin).
actor(dirtyrottenscoundrels,caine).
actor(shopgirl,danes).
actor(shopgirl,martin).

costar(X, X) :- false.
costar(X,Y) :- actor(Z, X), actor(Z, Y), not(X = Y).

busy(X) :- actor(Z, X), actor(Y, X), not(Y = Z). 

bacon(X, X) :- false.
bacon(X,Y) :- costar(X, Y).
bacon(X,Y) :- costar(X, ZT), bacon(ZT, ZN), costar(ZN,Y).


expr ::=
const(i)
| var(x)
| plus(expr , expr )
| leq(expr , expr )
| ite(expr , expr )
| letin(x, expr , expr )
| fun(var(x), expr )
| app(expr , expr )


type ::= int | bool | arrow(type, type)


envtype([], X, T) :- false.
envtype([[X,T]|_], X, T) :- !, true.
envtype([[A,B]|T1],X,T) :- envtype(T1, X, T).


% typeof(Env,const(I),T) :- T = const(_).
% typeof(Env,var(X),T) :- T = var(_).
% typeof(Env,plus(E1,E2),T) :- typeof(Env, E1, T), typeof(Env, E2, T).
% typeof(Env,leq(E1,E2),T) :- typeof(Env, E1, T), typeof(Env, E2, T).
% typeof(Env,ite(E1,E2,E3),T) :- typeof(Env, E1, T), typeof(Env, E2, T), typeof(Env, E3, T).
% typeof(Env,letin(var(X),E1,E2),T) :- typeof(Env, E2, var)
% typeof(Env,fun(var(X),E),T) :-
% typeof(Env,app(E1,E2),T) :- 









