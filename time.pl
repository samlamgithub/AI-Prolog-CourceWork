



time_stamp(X):- get_time(X).


time_diff(Diff):-
time_stamp(X),
do(100),
time_stamp(Y),
Diff is Y-X.

do(0):- V is 56874632+67532436475.

do(X):- 
X \= 0,
V is 56874632+67532436475,
Y is X-1,
do(Y). 