
time_stamp(X):- statistics(runtime, [Z| _]), X is Z*1000.


time_diff(Diff):-
statistics(runtime, [X,_]),
write(X),
nl,
do(10000),
statistics(runtime, [Y,_]),
write(Y),
Diff is Y-X,
write('Now is diff'),
nl,
format(' took ~20d sec.~n', [Diff]).

do(0):- V is 56874632+67532436475.

do(X):- 
X \= 0,
Y is X-1,
do(Y). 